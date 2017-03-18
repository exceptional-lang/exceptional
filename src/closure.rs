use binding_map::BindingMap;
use instructions::InstructionSequence;
use value::Value;
use std::rc::Rc;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub struct Closure {
    pub instructions: Rc<InstructionSequence>,
    pub parent_bindings: BindingMap,
}

impl Closure {
    pub fn new(instructions: Rc<InstructionSequence>, parent_bindings: &BindingMap) -> Closure {
        Closure {
            instructions: instructions,
            parent_bindings: parent_bindings.clone(),
        }
    }

    pub fn blank() -> Closure {
        Closure {
            instructions: Rc::new(vec![]),
            parent_bindings: BindingMap::new(None),
        }
    }

    pub fn init_map(&self, local_bindings: Vec<(String, Value)>) -> BindingMap {
        let map = BindingMap::new(Some(&self.parent_bindings));
        local_bindings.into_iter().fold(map, |mut map, (arg_name, value)| {
            map.local_assign(&arg_name, value);
            map
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;

    #[test]
    fn enclose_assigns_new_bidings() {
        let parent_map = BindingMap::new(None);
        let closure = Closure::new(Rc::new(vec![]), &parent_map);

        let new_map = closure.init_map(vec![("toto".to_owned(), v_number(1, 1))]);
        assert_eq!(new_map.fetch(&"toto".to_owned()), Some(v_number(1, 1)));
    }
}
