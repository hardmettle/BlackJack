package com.main

import com.boot.LoadGame
import com.members.{DEAL, HIT, STAND}

import scala.io.StdIn

object MainGame {
  def main(args: Array[String]): Unit = {
    println("Hello, welcome to the game of BLACKJACK!")
    println("Press X|x to exit any time in the game.")
    print("What would you like to keep your ACE value 1 or 11 ? ")

    while(true) {
      val aceValue = scala.io.StdIn.readLine()
      if (aceValue.toLowerCase() == "x") {
        println("Thanks for playing. Bye !")
        System.exit(0)
      } else if (aceValue.toCharArray.head != '1' || (aceValue.length == 2 && aceValue.toCharArray.head != '1' && aceValue.toCharArray.tail.head !=1) ) {
        println("Please enter either 1 or 11. Use x|X to exit. ")
      } else {
        println("Loading game... ")
        val init = LoadGame(aceValue.toInt)
        val init2 = init.updateGameStatus(DEAL("Dealer"),init)
        var game = init2.updateGameStatus(DEAL("Player"),init2)
        val r = scala.util.Random
        println(s"Game loaded.One of the card dealer has ${game.dealer.cards(r.nextInt(2))}")
        println(s"You have :")
        game.player.cards.foreach(c => println(c.name + " of " +c.value))
        var numberOfHitsRemaining = 5
        println(s"Make your choice. You have $numberOfHitsRemaining number of hits remaining")
        println("Press h to HIT or s to STAND")
        while(true){
          val choice = scala.io.StdIn.readLine().trim.toLowerCase()
          if (choice.length > 1) println("Please enter correct option.")
          if(choice.charAt(0) != 'x'){
            val h = choice.charAt(0) - 104
            val s =choice.charAt(0) - 115
            if(h != 0 && s != 0){
              println(s"h is $h")
              println(s"s is $s")
              println("Please choose the right option h|s|x ")
            }else{
              if(choice.charAt(0) == 'h' && numberOfHitsRemaining == 0){
                println("Sorry you have no hits remaining.")
                println(s"Dealer has :")
                game.dealer.cards.foreach(c => println(c.name + " of " +c.value))
                println(s"You have :")
                game.player.cards.foreach(c => println(c.name + " of " +c.value))
                println("The final outcome is : ")
                game.updateGameStatus(STAND(),game)
                System.exit(0)
              }else if(h == 0){
                game = game.updateGameStatus(HIT(None),game)
                numberOfHitsRemaining = numberOfHitsRemaining - 1
                println(s"You have ")
                game.player.cards.foreach(c => println(s"${c.name} of ${c.value}"))
                println(s"Make your next move. You have $numberOfHitsRemaining number of hits remaining")
              }else{
                println(s"Dealer has : ")
                game.dealer.cards.foreach(c => println(c.name + " of " +c.value))
                println(s"You have :")
                game.player.cards.foreach(c => println(c.name + " of " +c.value))
                println("The final outcome is : ")
                game.updateGameStatus(STAND(),game)
                System.exit(0)
              }
            }
          }else{
            println("Thanks for playing. Bye !")
            System.exit(0)
          }
        }
      }
    }
  }
}
