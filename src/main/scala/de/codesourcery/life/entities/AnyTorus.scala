package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A generic torus that stores it's values in a two-dimensional
 * object array.
 * 
 * @author Tobias.Gierke@code-sourcery.de
 */
class AnyTorus[T]( private val storage : TwoDimensionalStorage[T] ) extends Torus[T](storage) {

	def this(width:Int,height:Int) {
		this( new ArrayStorage[T](width,height) )
	}
	
	def createCopy() : AnyTorus[T] = {
		new AnyTorus[T]( data.createCopy )
	}
	
	def visitAlive( func : => (Int,Int) => Unit ) = throw new UnsupportedOperationException("Not implemented")

}

private class ArrayStorage[T](val w:Int,val h:Int) extends TwoDimensionalStorage[T](w,h) {
	
	private var data = new Array[Array[Any]](width)
	
	 def getValueAt(x:Int,y:Int) : T = {
		if ( data(x) == null )
			null.asInstanceOf[T]
		else
			data(x)(y).asInstanceOf[T]
	}
	
	def clear() {
		data = new Array[Array[Any]](w)
	}
	
	 def setValueAt(x:Int,y:Int,value:T) : T = {
		var ary = data(x)
		var oldValue : T = null.asInstanceOf[T]
		if (ary == null ) {
			ary = new Array[Any]( height )
			data(x)=ary
		} else {
			oldValue = ary(y).asInstanceOf[T]
		}
		ary(y)=value
		oldValue
	}
	 
	def createCopy() : TwoDimensionalStorage[T] = {
	 		val result = new ArrayStorage[T]( w , h )
	 		var width = 0
	 		while( width < w ) {
	 			if ( data(width) != null ) {
	 				result.data(width) = new Array[Any]( h )
	 				Array.copy( data(width) , 0 , result.data(width) , 0, height )
	 			}
	 			width+=1
	 		}
	 		result
	}
	
	def createCopy(newWidth:Int,newHeight:Int) : TwoDimensionalStorage[T] = {
	 		val result = new ArrayStorage[T]( newWidth , newHeight )
	 		var width = 0
	 		
	 		val maxWidth = if ( w <= newWidth) w else newWidth
	 		val minHeight = if ( h <= newHeight ) h else newHeight
	 		while( width < maxWidth ) {
	 			if ( data(width) != null ) {
	 				result.data(width) = new Array[Any]( newHeight )
	 				Array.copy( data(width) , 0 , result.data(width) , 0, minHeight )
	 			}
	 			width+=1
	 		}
	 		result
	}	
}