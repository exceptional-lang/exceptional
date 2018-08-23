use ast::*;
use instructions::*;
use std::rc::Rc;

fn compile_statement(statement: &Statement) -> InstructionSequence {
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
        &Statement::Call(ref target, ref expressions) => {
            let mut instructions = expressions
                .iter()
                .flat_map(|exp| compile_expression(exp))
                .collect::<InstructionSequence>();
            instructions.extend(compile_expression(target).iter().cloned());
            instructions.push(Instruction::Call(expressions.len()));
            instructions
        }
        &Statement::Rescue(ref pattern, ref statements) => {
            let instructions = statements
                .iter()
                .flat_map(|stmt| compile_statement(stmt))
                .collect::<InstructionSequence>();

            vec![
                Instruction::Rescue(Rc::new(pattern.clone()), Rc::new(instructions)),
            ]
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

fn compile_expression(expression: &Expression) -> InstructionSequence {
    match expression {
        &Expression::Literal(ref literal) => match literal {
            &Literal::Map(ref pairs) => {
                let mut map_instructions = pairs
                    .iter()
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
        },
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
        &Expression::Import(ref name) => {
            let mut instructions = compile_expression(name);
            instructions.push(Instruction::Import);
            instructions
        }
    }
}

fn compile_binop(op: &str) -> Instruction {
    match op {
        "+" => Instruction::BinOp(Op::Add),
        "-" => Instruction::BinOp(Op::Sub),
        "/" => Instruction::BinOp(Op::Div),
        "*" => Instruction::BinOp(Op::Mul),
        _ => panic!("Unsupported binary operation: {:?}", op),
    }
}

pub fn compile(statements: &Vec<Statement>) -> InstructionSequence {
    let mut instructions = InstructionSequence::new();

    for statement in statements.iter() {
        instructions.push(Instruction::Clear);
        instructions.extend(compile_statement(&statement).iter().cloned());
    }

    instructions
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;

    #[test]
    fn compiles_binop() {
        assert_eq!(compile_binop("+"), Instruction::BinOp(Op::Add));
        assert_eq!(compile_binop("-"), Instruction::BinOp(Op::Sub));
        assert_eq!(compile_binop("/"), Instruction::BinOp(Op::Div));
        assert_eq!(compile_binop("*"), Instruction::BinOp(Op::Mul));
    }

    #[test]
    fn compiles_literal_expressions() {
        assert_eq!(
            compile_expression(&e_literal(l_number(1, 1))),
            vec![Instruction::Push(Literal::Number(build_ratio(1, 1)))]
        )
    }

    #[test]
    fn compiles_binop_expressions() {
        assert_eq!(
            compile_expression(&e_binop(
                "+",
                e_literal(l_number(1, 1)),
                e_literal(l_number(2, 1)),
            )),
            vec![
                Instruction::Push(Literal::Number(build_ratio(1, 1))),
                Instruction::Push(Literal::Number(build_ratio(2, 1))),
                Instruction::BinOp(Op::Add),
            ]
        )
    }

    #[test]
    fn compiles_identifier_expressions() {
        assert_eq!(
            compile_expression(&e_identifier("toto")),
            vec![Instruction::Fetch("toto".to_owned())]
        );
    }

    #[test]
    fn compiles_index_access_expressions() {
        assert_eq!(
            compile_expression(&e_index_access(
                e_identifier("toto"),
                e_literal(l_string("titi")),
            )),
            vec![
                Instruction::Fetch("toto".to_owned()),
                Instruction::Push(l_string("titi")),
                Instruction::IndexAccess,
            ]
        )
    }

    #[test]
    fn compiles_import_expressions() {
        assert_eq!(
            compile_expression(&e_import(e_literal(l_string("toto")))),
            vec![Instruction::Push(l_string("toto")), Instruction::Import]
        )
    }
}
