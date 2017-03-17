use ast::*;
use vm::*;
use std::rc::Rc;

fn compile_statement<'a>(statement: &'a Statement) -> InstructionSequence {
    // TODO: Clear stack at start of statement?
    match statement {
        &Statement::Assign(local, ref binding_name, ref expression) => {
            let mut instructions = compile_expression(expression);
            let instruction = match local {
                true => Instruction::LocalAssign(binding_name.to_owned()),
                false => Instruction::Assign(binding_name.to_owned()),
            };
            instructions.push(instruction);
            instructions
        }
        &Statement::Call(ref binding_name, ref expressions) => {
            let mut instructions = expressions.iter()
                .flat_map(|exp| compile_expression(exp))
                .collect::<InstructionSequence>();
            instructions.push(Instruction::Fetch(binding_name.to_owned()));
            instructions.push(Instruction::Call(expressions.len()));
            instructions
        }
        &Statement::Rescue(ref pattern, ref statements) => {
            let instructions = statements.iter()
                .flat_map(|stmt| compile_statement(stmt))
                .collect::<InstructionSequence>();

            vec![Instruction::Rescue(Rc::new(pattern.clone()), Rc::new(instructions))]
        }
        &Statement::Raise(ref expression) => {
            let mut instructions = compile_expression(expression);
            instructions.push(Instruction::Raise);
            instructions
        }
        &Statement::IndexAssign(ref target, ref property, ref value) => {
            let mut instructions = compile_expression(target);
            instructions.extend(compile_expression(property).iter().cloned());
            instructions.extend(compile_expression(value).iter().cloned());
            instructions.push(Instruction::IndexAssign);
            instructions
        }
        //        s => panic!("not implemented: {:?}", s),
    }
}

fn compile_expression<'a>(expression: &'a Expression) -> InstructionSequence {
    match expression {
        &Expression::Literal(ref literal) => {
            match literal {
                &Literal::Map(ref pairs) => {
                    let mut map_instructions = pairs.iter()
                        .flat_map(|&(ref key, ref value)| {
                            let mut insns = compile_expression(key);
                            insns.extend(compile_expression(value).iter().cloned());
                            insns
                        })
                        .collect::<InstructionSequence>();

                    map_instructions.push(Instruction::MakeMap(pairs.len()));
                    map_instructions
                }
                _ => vec![Instruction::Push(literal.to_owned())],
            }
        }
        &Expression::Identifier(ref binding_name) => {
            vec![Instruction::Fetch(binding_name.to_owned())]
        }
        &Expression::BinOp(ref op, ref left, ref right) => {
            let mut instructions = compile_expression(&*left);
            instructions.extend(compile_expression(&*right).iter().cloned());
            instructions.push(compile_binop(&*op));
            instructions
        }
        &Expression::IndexAccess(ref target, ref property) => {
            let mut instructions = compile_expression(&*target);
            instructions.extend(compile_expression(&*property).iter().cloned());
            instructions.push(Instruction::IndexAccess);
            instructions
        }
    }
}

fn compile_binop(op: &str) -> Instruction {
    match op {
        "+" => Instruction::Add,
        "-" => Instruction::Sub,
        "/" => Instruction::Div,
        "*" => Instruction::Mul,
        _ => panic!("Unsupported binary operation: {:?}", op),
    }
}

pub fn compile<'a>(statements: &'a Vec<Statement>) -> InstructionSequence {
    let mut instructions = InstructionSequence::new();

    for statement in statements.iter() {
        instructions.extend(compile_statement(&statement).iter().cloned());
    }

    instructions
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;
    use std::rc::Rc;

    #[test]
    fn compiles_binop() {
        assert_eq!(compile_binop("+"), Instruction::Add);
        assert_eq!(compile_binop("-"), Instruction::Sub);
        assert_eq!(compile_binop("/"), Instruction::Div);
        assert_eq!(compile_binop("*"), Instruction::Mul);
    }

    #[test]
    #[ignore]
    fn compiles_literal_expressions() {
        assert_eq!(compile_expression(&e_literal(l_number(1, 1))),
                   vec![Instruction::Push(Literal::Number(build_ratio(1, 1)))])
    }

    #[test]
    #[ignore]
    fn compiles_binop_expressions() {
        assert_eq!(compile_expression(&e_binop("+",
                                               e_literal(l_number(1, 1)),
                                               e_literal(l_number(2, 1)))),
                   vec![Instruction::Push(Literal::Number(build_ratio(1, 1))),
                        Instruction::Push(Literal::Number(build_ratio(1, 1))),
                        Instruction::Add])
    }

    #[test]
    #[ignore]
    fn compiles_identifier_expressions() {
        assert_eq!(compile_expression(&e_identifier("toto")),
                   vec![Instruction::Fetch("toto".to_owned())]);
    }

    #[test]
    #[ignore]
    fn compiles_index_access_expressions() {
        assert_eq!(compile_expression(&e_index_access(e_identifier("toto"),
                                                      e_literal(l_string("titi")))),
                   vec![Instruction::Fetch("toto".to_owned()),
                        Instruction::Push(l_string("titi")),
                        Instruction::IndexAccess])
    }
}
