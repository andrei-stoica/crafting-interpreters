package com.craftinginterpreters.lox;

import java.util.List;

public class LoxFunction implements LoxCallable {
  private final Stmt.Function decleration;
  private final Environment closure;

  LoxFunction(Stmt.Function decleration, Environment closure) {
    this.closure = closure;
    this.decleration = decleration;
  }

  @Override
  public Object call(Interpreter interpreter, List<Object> arguments) {
    Environment environment = new Environment(closure);
    for (int i = 0; i < arguments.size(); i++) {
      environment.define(decleration.params.get(i).lexeme, arguments.get(i));
    }

    try {
      interpreter.executeBlock(decleration.body, environment);
    } catch (Return returnValue) {
      return returnValue.value;
    }
    return null;
  }

  @Override
  public int arity() {
    return decleration.params.size();
  }

  @Override
  public String toString() {
    return "<fn " + decleration.name.lexeme + ">";
  }
}
