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
      } else if (!aceValue.equals("11") && !aceValue.equals("1")) {
        println("Please enter either 1 or 11. Use x|X to exit. ")
      } else {
        println("Loading game... ")
        val init = LoadGame(aceValue.toInt)
        val init2 = init.updateGameStatus(DEAL("Dealer"),init)
        var game = init2.updateGameStatus(DEAL("Player"),init2)
        val r = scala.util.Random
        val dealerCard = game.dealer.cards(r.nextInt(2))
        println(s"Game loaded.One of the card dealer has $dealerCard")
        println(s"You have :")
        game.player.cards.foreach(c => println(c.name + " of " +c.value))
        var numberOfHitsRemaining = 5
        println(s"Make your choice HIT or STAND. You have $numberOfHitsRemaining number of hits remaining")
        game.gamePlay(numberOfHitsRemaining)
        System.exit(0)
      }
    }
  }
}
