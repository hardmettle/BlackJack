package com.boot

import com.members.{BlackJack, Card, CardDeck, Player}

/**
  * Created by harsh on 07/01/17.
  */
object LoadGame {

  val player = Player("Player",List.empty[Card])
  val dealer = Player("Dealer",List.empty[Card])

  val nameOfCard = List("JACK","QUEEN","KING")
  val ordinaryCard = "ORDINARY"
  val aceCard = "ACE"
  val patternOfCard = List("DIAMOND","SPADE","CLUB","HEART")
  val numberOfCard = List(2,3,4,5,6,7,8,9,10)

  def apply(aceValue:Int):BlackJack = {

    val acePattern = for { x <- patternOfCard } yield s"$aceCard $x"
    val aceCards = acePattern.map(a => Card(a,aceValue))
    aceCards.foreach(a => println(s"Loading $a"))

    val combinedNamePattern = for { x <- nameOfCard; y <- patternOfCard } yield s"$x $y"
    val nameCards = combinedNamePattern.map(c => Card(c,10))
    nameCards.foreach(n => println(s"Loading $n"))

    val ordinaryCards = for { x <- numberOfCard; y <- patternOfCard } yield Card(s"$ordinaryCard $y",x)
    ordinaryCards.foreach(o => println(s"Loading $o"))
    val cardDeck = CardDeck(aceCards ++ nameCards ++ ordinaryCards)
    BlackJack(player,dealer,cardDeck)
  }
}