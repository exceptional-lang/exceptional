use value::Value;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub struct BindingMap {
    map: Rc<RefCell<BTreeMap<String, Value>>>,
    parent: Option<Box<BindingMap>>,
}

impl BindingMap {
    pub fn new(parent: Option<&BindingMap>) -> BindingMap {
        let parent = match parent {
            Some(map) => Some(Box::new(map.clone())),
            None => None,
        };
        BindingMap {
            map: Rc::new(RefCell::new(BTreeMap::new())),
            parent: parent,
        }
    }

    pub fn fetch(&self, binding_name: &String) -> Option<Value> {
        match self.map.borrow().get(binding_name) {
            Some(value) => Some(value.clone()),
            None => {
                match self.parent {
                    Some(ref parent) => parent.fetch(binding_name),
                    None => None,
                }
            }
        }
    }

    pub fn local_assign(&mut self, binding_name: &String, value: Value) {
        self.map
            .borrow_mut()
            .insert(binding_name.to_owned(), value);
    }

    pub fn assign(&mut self, binding_name: &String, value: Value) {
        match self.has_binding(binding_name) {
            true => self.local_assign(binding_name, value),
            false => {
                match self.parent {
                    Some(ref mut parent) => parent.assign(binding_name, value),
                    None => panic!("no such binding"),
                }
            }
        }
    }

    pub fn has_binding(&self, binding_name: &String) -> bool {
        self.map.borrow().contains_key(binding_name)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;
    use std::collections::BTreeMap;
    use std::rc::Rc;
    use std::cell::RefCell;

    #[test]
    fn new() {
        assert_eq!(Rc::new(RefCell::new(BTreeMap::new())),
                   BindingMap::new(None).map)
    }

    #[test]
    fn has_binding() {
        let map = BindingMap::new(None);
        assert_eq!(false, map.has_binding(&"toto".to_owned()))
    }

    #[test]
    fn local_assign() {
        let mut map = BindingMap::new(None);
        map.local_assign(&"toto".to_owned(), v_string("value"));
        assert!(map.has_binding(&"toto".to_owned()))
    }

    #[test]
    fn fetch() {
        let mut map = BindingMap::new(None);
        map.local_assign(&"toto".to_owned(), v_string("value"));
        assert_eq!(Some(v_string("value")), map.fetch(&"toto".to_owned()))
    }

    #[test]
    fn delegates_fetch() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), v_string("value"));
        let map = BindingMap::new(Some(&parent));

        assert_eq!(Some(v_string("value")), map.fetch(&"toto".to_owned()))
    }

    #[test]
    fn delegates_assign() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), v_string("value"));
        let mut map = BindingMap::new(Some(&parent));
        map.assign(&"toto".to_owned(), v_string("new_value"));

        assert_eq!(Some(v_string("new_value")),
                   parent.fetch(&"toto".to_owned()))
    }
}
