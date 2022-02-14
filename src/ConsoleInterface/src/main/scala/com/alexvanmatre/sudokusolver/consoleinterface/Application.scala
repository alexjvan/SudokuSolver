package com.alexvanmatre.sudokusolver.consoleinterface

object Application extends App {

  if (args.length < 0 || args.length > 3)
      println("Incorrect usage ("+args.length+" args)! Correct usage java -jar ConsoleInterface-1.0-SNAPSHOT.jar <path to puzzle> [statistics=true|false]")
  else {
      if(args.length == 1)
          new Interface(args(0), false)
      else if(args.length == 2)
          new Interface(args(0), args(1).toBoolean)
  }

}
