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

import com.members

import scala.util.Random
case class BlackJack(player: Player,dealer:Player,cardDeck: CardDeck){

  def gamePlay(hitsRemaining:Int) = {
    this.player.makeAMove(hitsRemaining,this.cardDeck.cards) match {
      case h@HIT(_) => {
        this.updateGameStatus(h,this)
        println(s"Player has chosen HIT. Now he has :")
        this.player.cards.foreach(c => println(s"${c.name} of ${c.value}"))
        this.player.makeAMove(hitsRemaining-1,this.cardDeck.cards)
      }
      case STAND() => {
        println(s"Dealer has : ")
        this.dealer.cards.foreach(c => println(c.name + " of " +c.value))
        println(s"You have :")
        this.player.cards.foreach(c => println(c.name + " of " +c.value))
        println("The final outcome is : ")
        this.updateGameStatus(STAND(),this)
      }
    }
  }
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
    val currentCards = this.cardDeck.cards
    val hitValue = currentCards.head
    val optionHitValue = Some(hitValue)
    val updatedPlayer = this.player.action(HIT(optionHitValue))
    val updatedDeck = this.cardDeck.action(UPDATE_DECK(currentCards,HIT(optionHitValue)))
    this.copy(player = updatedPlayer,cardDeck = updatedDeck)
  }
  private def dealCard(deal: DEAL):BlackJack = {
    def crookedDeal():(Card,Card,List[Card]) = {
      val cardsWithNine = this.cardDeck.cards.filter(c => c.value == 9)
      val cardsWithTen = this.cardDeck.cards.filter(c => c.value == 10)
      val cardsWithEleven = this.cardDeck.cards.filter(c => c.value == 11)
      val restOfTheCards = this.cardDeck.cards.filterNot(c => c.value == 11 || c.value == 10 || c.value == 9)
      val possibleCards = cardsWithNine ++ cardsWithTen ++ cardsWithEleven
      val r = scala.util.Random
      val randomIndex = r.nextInt(possibleCards.size)
      val randomSelection = possibleCards(randomIndex)
      val remainingTarget = 20 - randomSelection.value
      val remainingPossibleCards = possibleCards.take(randomIndex) ++ possibleCards.drop(randomIndex+1)
      val possibleCardsTwo = remainingPossibleCards.filter(c => c.value - remainingTarget == 0)
      val randomIndexTwo = r.nextInt(possibleCardsTwo.size)
      val randomSelectionTwo = possibleCardsTwo(randomIndexTwo)
      val unUsedRemainingDeck = remainingPossibleCards.filterNot(c => randomSelectionTwo == c)

      (randomSelection,randomSelectionTwo,List(randomSelection,randomSelectionTwo) ++
        Random.shuffle(unUsedRemainingDeck ++ restOfTheCards))
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
  def makeAMove(hitsRemaining:Int,cardsInDeck:List[Card]):Instruction = {
    if(hitsRemaining != 0){
      if(getProbablityForNextMove(hitsRemaining,cardsInDeck) > 0.5){
        members.HIT(None)
      }else{
        STAND()
      }
    } else {
      members.STAND()
    }
  }
  private def getProbablityForNextMove(hitsRemaining:Int,cardsInDeck:List[Card]): Double = {
    val total = this.cards.map(_.value).sum
    def powerSet(t: List[Int]): List[List[Int]] = {
      @annotation.tailrec
      def pwr(t: List[Int], ps: List[List[Int]]): List[List[Int]] =
        if (t.isEmpty) ps
        else pwr(t.tail, ps ++ (ps map (_ :+ t.head) ))
      pwr(t, List(List.empty[Int]))
    }
    val allCombinations = powerSet(cardsInDeck.map(_.value))//sample
    val remainingSum = 21 - total
    val favourableChoices = (1 to hitsRemaining).map(hits => {
      allCombinations.filter(_.size == hits).map(_.sum).count(_ == remainingSum)
    })//choices per hits
    val allCombinationSize = allCombinations.size
    val probability = favourableChoices.map(_.toDouble).reduce((f1,f2) => {
      f1.toDouble / allCombinationSize.toDouble + f2.toDouble / allCombinationSize.toDouble
    })
    println(s"Probability is $probability")
    probability
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
