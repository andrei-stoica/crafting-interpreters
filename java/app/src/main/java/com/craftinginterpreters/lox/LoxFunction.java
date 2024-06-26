package com.craftinginterpreters.lox;

import java.util.List;

public class LoxFunction implements LoxCallable {
  private final Stmt.Function decleration;
  private final Environment closure;
  private final boolean isInitializer;

  LoxFunction(Stmt.Function decleration, Environment closure, boolean isInitializer) {
    this.closure = closure;
    this.decleration = decleration;
    this.isInitializer = isInitializer;
  }

  LoxFunction bind(LoxInstance instance) {
    Environment environment = new Environment(closure);
    environment.define("this", instance);
    return new LoxFunction(decleration, environment, isInitializer);
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
      if (isInitializer) {
        return closure.getAt(0, "this");
      }
      return returnValue.value;
    }
    if (isInitializer) {
      return closure.getAt(0, "this");
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
