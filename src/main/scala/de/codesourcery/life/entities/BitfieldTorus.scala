package de.codesourcery.life.entities

import scala.collection.mutable.ArrayBuffer

/**
 * A torus that holds boolean values and uses a bitfield as it's underlying
 * storage.
 * 
 * @author Tobias.Gierke@code-sourcery.de
 */
class BitfieldTorus( private val stuff : BitfieldStorage  ) extends Torus[Boolean](stuff) {

	def this(width:Int,height:Int) {
		this( new BitfieldStorage(width,height) )
	}
	
	def createCopy() : BitfieldTorus = {
		new BitfieldTorus( data.createCopy().asInstanceOf[BitfieldStorage] )
	}
	
	def visitAlive( func : => (Int,Int) => Unit ) {
		data.asInstanceOf[BitfieldStorage].visitAlive( func )
	}
}

class BitfieldStorage(val w:Int,val h:Int) extends TwoDimensionalStorage[Boolean](w,h) {
	
	// number of bits per array element
	private val ARRAY_ELEMENT_BIT_WIDTH = 32 // update SHIFT_BITCOUNT as well when changing this
	
	// I use shift operations instead of divide / multiply
	private val SHIFT_BITCOUNT = 5 // = 2^5 = 32 = ARRAY_ELEMENT_BIT_WIDTH
	
	// the width needs to be a multitude of ARRAY_ELEMENT_BIT_WIDTH (read: 2^X)
	// so we might need to align the width provided by the client for our internal use 
	private val paddedWidth : Int =  if ( ( w % ARRAY_ELEMENT_BIT_WIDTH ) != 0 ) {
		val tmp :Int = Math.ceil( w.asInstanceOf[Float] / ARRAY_ELEMENT_BIT_WIDTH.asInstanceOf[Float] ).asInstanceOf[Int]
		tmp * ARRAY_ELEMENT_BIT_WIDTH
	} else {
		w 
	}
	
	private var data : Array[Int]= new Array[Int]( calcLength(w,h ) )
	
	// calculate data array element size (length) using the correctly
	// aligned width
	private def calcLength(aWidth : Int, aHeight : Int ) : Int = {
		Math.ceil( ( aHeight * paddedWidth ) / ARRAY_ELEMENT_BIT_WIDTH ).asInstanceOf[Int]
	}
	
	def visitAlive( func : => (Int,Int) => Unit ) {
		
		var bitOffset = 0
		var index = 0
		val len = data.length
		
		while ( index < len ) {
			val value = data(index)
			if ( value != 0 ) 
			{ // at least one bit is set
				var currentOffset = bitOffset
				var currentBit = 0
				while ( currentBit < ARRAY_ELEMENT_BIT_WIDTH ) 
				{
					if ( ( value & ( 1 << currentBit ) ) != 0 ) {
						val y = currentOffset / paddedWidth
						val x = currentOffset % paddedWidth
						func( x , y )
					}
					currentBit += 1
					currentOffset += 1
				}
			}
			bitOffset += ARRAY_ELEMENT_BIT_WIDTH
			index += 1
		}
	}
	
	def clear() {
		data = new Array[Int]( calcLength(w,h ) )
	}
	
	def getValueAt(x:Int,y:Int) : Boolean = {
		
		// note: the bit fiddling code in getValueAt() / setValueAt() is 
		// intentionally copied instead of put into a function because
		// we would need to return a tuple (byteOffset,offsetInByte)
		// and there's just no way the compiler can be 
		// smart enough to avoid the tuple creation
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset >>> SHIFT_BITCOUNT //   read: bitOffset / ARRAY_ELEMENT_BIT_WIDTH 
		val offsetInByte : Int = bitOffset - ( byteOffset  << SHIFT_BITCOUNT ) // read: byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		return ( data(byteOffset) & ( 1 << offsetInByte ) ) != 0
	}
	
	def setValueAt(x:Int,y:Int,value:Boolean) : Boolean = {
		
		val bitOffset : Int = x + y* paddedWidth
		val byteOffset : Int = bitOffset >>> SHIFT_BITCOUNT //   read: bitOffset / ARRAY_ELEMENT_BIT_WIDTH
		val offsetInByte : Int = bitOffset - ( byteOffset  << SHIFT_BITCOUNT ) // read: byteOffset * ARRAY_ELEMENT_BIT_WIDTH )
		
		val oldValue = ( data(byteOffset) & ( 1 << offsetInByte ) ) != 0
		
		if ( value ) { // set bit
			data(byteOffset) = ( data(byteOffset) | 1 << offsetInByte)
		} else { // clear bit
			data(byteOffset) = (data(byteOffset) & ~(1 << offsetInByte) )
		}
		oldValue
	}
	 
	def createCopy() : BitfieldStorage = {
	 		val result = new BitfieldStorage( w , h )
	 		Array.copy( data , 0 , result.data , 0 , data.length )
	 		result
	}
	
	def createCopy(newWidth:Int,newHeight:Int) : TwoDimensionalStorage[Boolean] = {

 		val result = new BitfieldStorage( newWidth , newHeight )

 		val copyFunction :  (Int,Int,Boolean)  => Unit = (x,y,isSet) => {
 			if ( isSet && ( x < newWidth && y < newHeight ) ) {
 				result.setValueAt( x , y , true )
 			}
 		}
 	    visitAll( copyFunction );
 		result
	}	
}