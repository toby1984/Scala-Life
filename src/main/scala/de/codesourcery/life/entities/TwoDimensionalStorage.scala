package de.codesourcery.life.entities

abstract class TwoDimensionalStorage[T](val width:Int,val height:Int) {

	def getValueAt(x:Int,y:Int) : T
	def setValueAt(x:Int,y:Int,value:T) : Unit
	def createCopy() : TwoDimensionalStorage[T]
	
}