package com.craftinginterpreters.lox;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Lox {
  static boolean hadError = false;

  public static void main(String[] args) throws IOException {
    System.out.println("test");
    if (args.length > 1) {
      System.out.println("Usage: jlox [script]");
    } else if (args.length == 1) {
      runFile(args[0]);
    } else {
      runPrompt();
    }
  }

  public static void runFile(String path) throws IOException {
    byte[] bytes = Files.readAllBytes(Paths.get(path));
    run(new String(bytes, Charset.defaultCharset()));
    if (hadError) {
      System.exit(65);
    }
  }

  public static void runPrompt() throws IOException {
    InputStreamReader input = new InputStreamReader(System.in);
    BufferedReader reader = new BufferedReader(input);

    for (; ; ) {
      System.out.print("> ");
      String line = reader.readLine();
      if (line == null) {
        break;
      }
      run(line);
      hadError = false;
    }
  }

  public static void run(String source) {
    AstPrinter astPrinter = new AstPrinter();
    Scanner scanner = new Scanner(source);
    List<Token> tokens = scanner.scanTokens();
    Parser parser = new Parser(tokens);
    Expr expression = parser.parse();

    if (hadError) {
      return;
    }
    System.out.println(astPrinter.print(expression));
  }

  public static void error(int line, String message) {
    report(line, "", message);
  }

  public static void error(Token token, String message) {
    if (token.type == TokenType.EOF) {
      report(token.line, " at end", message);
    } else {
      report(token.line, " at '" + token.lexeme + "'", message);
    }
  }

  public static void report(int line, String where, String message) {
    System.out.println("[line " + line + "] Error" + where + ": " + message);
  }
}
