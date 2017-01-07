package com.members

/**
  * Created by harsh on 07/01/17.
  */


sealed trait Instruction
case class DEAL(name:String) extends Instruction
case class HIT(card: Option[Card]) extends Instruction
case class STAND() extends Instruction
case class SHOW_VALUE() extends Instruction
case class UPDATE_DECK(currentCards:List[Card],instruction: Instruction) extends Instruction
case class UPDATE_PLAYER(newCardOne:Card,newCardTwo:Card) extends Instruction

import scala.util.Random
case class BlackJack(player: Player,dealer:Player,cardDeck: CardDeck){

  def updateGameStatus(instruction: Instruction,blackJack: BlackJack):BlackJack = {
    instruction match {
      case h@HIT(_) => this.hitCard()
      case d@DEAL(_) => this.dealCard(d)
      case STAND() => println(outCome());blackJack
    }
  }

  private def outCome():String = {
    val playerCardValue = this.player.cards.map(_.value).sum
    if(playerCardValue == 21) "YOU WIN !!" else "SORRY ! YOU LOST :( ."
  }

  private def hitCard():BlackJack = {
    val shuffledCards = Random.shuffle(this.cardDeck.cards)
    val hitValue = shuffledCards.head
    val optionHitValue = Some(hitValue)
    val updatedPlayer = this.player.action(HIT(optionHitValue))
    val updatedDeck = this.cardDeck.action(UPDATE_DECK(shuffledCards,HIT(optionHitValue)))
    this.copy(player = updatedPlayer,cardDeck = updatedDeck)
  }
  private def dealCard(deal: DEAL):BlackJack = {
    def crookedDeal():(Card,Card,List[Card]) = {
      val cardsWithTen = this.cardDeck.cards.filter(c => c.value == 10)
      val restOfTheCards = this.cardDeck.cards.filterNot(c => c.value == 10)
      val shuffled = Random.shuffle(cardsWithTen)
      val firstCard = shuffled.head
      val shuffledTwo = Random.shuffle(shuffled.tail)
      val secondCard = shuffledTwo.head
      (firstCard,secondCard,List(firstCard,secondCard) ++ Random.shuffle(Random.shuffle(shuffledTwo) ++ restOfTheCards))
    }
    val (updatedPlayer,shuffledCards) = if(deal.name == "Player"){
      val honestShuffledCards = Random.shuffle(this.cardDeck.cards)
      val (cardOne,cardTwo) = (honestShuffledCards.head,honestShuffledCards.tail.head)
      (this.player.action(UPDATE_PLAYER(cardOne,cardTwo)),honestShuffledCards)
    }else{
      val (cardOne,cardTwo,crookedShuffledCards) =  crookedDeal()
      (this.dealer.action(UPDATE_PLAYER(cardOne,cardTwo)),crookedShuffledCards)
    }
    val updatedDeck = this.cardDeck.action(UPDATE_DECK(shuffledCards,deal))
    val updatedGame = if(deal.name == "Player")
    this.copy(player = updatedPlayer,cardDeck = updatedDeck)
    else this.copy(dealer=updatedPlayer,cardDeck=updatedDeck)
    updatedGame
  }
}

trait Members {
  val name:String
  def action(instruction: Instruction):Members
}

case class Player(name:String,cards:List[Card]) extends Members{
  override def action(instruction: Instruction):Player = instruction match {
    case HIT(c) => this.copy(cards = cards :+ c.get)
    case UPDATE_PLAYER(one,two) => this.copy(cards=List(one,two))
  }
}

case class CardDeck(cards:List[Card],name:String = "BLACKJACK_DECK") extends Members{
  override def action(instruction: Instruction):CardDeck = instruction match {
    case u@UPDATE_DECK(cc,i) => i match {
      case HIT(_) => this.copy(cards = cc.tail)
      case DEAL(_) => this.copy(cards = cc.tail.tail)
    }
  }
}

case class Card(name:String,value:Int)
