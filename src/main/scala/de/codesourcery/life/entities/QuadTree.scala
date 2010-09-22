package de.codesourcery.life.entities

class QuadTree(private val w : Int, private val h : Int) extends TwoDimensionalStorage[Boolean](w,h) {

		/**
	 * Returns the value at a given X/Y coordinate.
	 * 
	 * @param x X coordinate (range 0...height-1)
	 * @param y Y coordinate (range 0...width-1)
	 * @return the value
	 * @thows ArrayIndexOutOfBoundsException if the coordinates
	 * are beyond the actual extend (less than zero
	 * or equals or greater than width / height )
	 */
	def getValueAt(x:Int,y:Int) : Boolean = {
		
	}
	
	/**
	 * Stores a value at a given X/Y coordinate.
	 * 
	 * @param x X coordinate (range 0...height-1)
	 * @param y Y coordinate (range 0...width-1)
	 * @param value the value to store
	 * @thows ArrayIndexOutOfBoundsException if the coordinates
	 * are beyond the actual extend (less than zero
	 * or equals or greater than width / height )
	 */	
	def setValueAt(x:Int,y:Int,value:Boolean)  {
		
	}
	
	/**
	 * Creates an independent copy of this
	 * instance.
	 * 
	 * @return
	 */
	def createCopy() : TwoDimensionalStorage[Boolean] = {
		
	}
	
	/**
	 * Creates an independent copy of this
	 * instance with a specific size.
	 *
	 * <p>If the target is smaller than this
	 * storage, this method will ignore
	 * all elements that do not fall within the 
	 * bounds of the target.</p
	 * @return
	 */
	def createCopy(width:Int,height:Int) : TwoDimensionalStorage[Boolean]	= {
		
	}
	
    /**
	 * Resets this storage's content
	 * to it's initial state.
	 */
	def clear() {
		
	}
}

class Envelope(val x1 : Double, val y1 : Double , val x2 : Double, val y2 : Double) 
{
	def contains(x : Int , y : Int ) : Boolean = {
		return ( x >= x1 && x <= x2 ) && ( y >= y1 && y <= y2 ); 
	}
	
	def equals(x:Int,y:Int) : Boolean = {
		return x1 == x && x2 == x && y1 == y && y2 == y
	}
}

class TreeNode(val parent : TreeNode , val envelope : Envelope, var value: Boolean ) {
	
	/*
	 * Quadrants:
	 *         |
	 *    I    |    II
	 * --------+---------
	 *   III   |    VI
	 *         |
	 */
	
	private val quadrants = new Array[TreeNode](4)
	
	private def getNode(index:Int) : TreeNode = {
		return quadrants(index)
	}
	
	def contains(x:Int,y:Int) : Boolean = {
		return envelope.contains(x,y)
	}
	
	private def createEnvelope(quadrant:Int) : Envelope = {
		
	/*
	 * Quadrants:
	 *           |
	 *    I (0)  |    II (1)
	 * ----------+---------
	 *   III (2) |    IV (3)
	 *           |
	 */

		val first = getNode(1)		
		val second = getNode(1)
		val third = getNode(2)
		val fourth = getNode(3)		
		
		quadrant match {
			case 0 => { // quadrant I
				val x1 = envelope.x1 
				val y1 = envelope.y1 
				
				var x2 = 0.0d
				var y2 = 0.0d
				
				if ( second != null ) {
					x2 = second.envelope.x1
					y2 = second.envelope.y2
				} else if ( third != null ){
					x2 = third.envelope.x2
					y2 = third.envelope.y1				
				} else if ( fourth != null ){
					x2 = fourth.envelope.x1
					y2 = fourth.envelope.y1
				} else {
					x2 = x1 + ( ( envelope.x2 - envelope.x1  ) / 2.0 )
					y2 = y1 + ( ( envelope.y2 - envelope.y1  ) / 2.0 )				
				}
				new Envelope(x1,y1,x2,y2)
			}
			case 1 => { // quadrant II
				var x1 = 0.0d
				val y1 = envelope.y1
				
				val x2 = envelope.x2
				var y2 = 0.0d
				
				if ( first != null ) {
					x1 = first.envelope.x2
					y2 = first.envelope.y2
				} else if ( third != null ){
					x1 = third.envelope.x2
					y2 = third.envelope.y1				
				} else if ( fourth != null ){
					x1 = fourth.envelope.x1
					y2 = fourth.envelope.y1
				} else {
					x1 = envelope.x1 + ( ( envelope.x2 - envelope.x1  ) / 2.0 )
					y2 = y1 + ( ( envelope.y2 - envelope.y1  ) / 2.0 )				
				}
				new Envelope(x1,y1,x2,y2)				
			}
			case 2 => { // quadrant III
				val x1 = envelope.x1
				var y1 = 0.0d
				
				var x2 = 0.0d
				val y2 = envelope.y2
				
				if ( first != null ) {
					y1 = first.envelope.y2
					x2 = first.envelope.x2
				} else if ( second != null ){
					y1 = second.envelope.y2
					x2 = second.envelope.x1
				} else if ( fourth != null ){
					y1 = envelope.y2
					x2 = fourth.envelope.x1
				} else {
					y1 = envelope.y1
					x2 = envelope.x1 + ( ( envelope.x2 - envelope.x1  ) / 2.0 )					
				}
				new Envelope(x1,y1,x2,y2)					
			}
			case 3 => { // quadrant IV
				var x1 = 0.0d
				var y1 = 0.0d
				
				val x2 = envelope.x2
				val y2 = envelope.y2
				
				if ( first != null ) {
					x1 = envelope.x1
					y1 = first.envelope.y2
				} else if ( second != null ){
					x1 = envelope.x1
					y1 = second.envelope.y2
				} else if ( fourth != null ){
					x1 = envelope.x1
					y1 = envelope.y2
				} else {
					x1 = envelope.x1 + ( ( envelope.x2 - envelope.x1 ) / 2.0 )
					y1 = envelope.y1 + ( ( envelope.y2 - envelope.y1 ) / 2.0 )
				}
				new Envelope(x1,y1,x2,y2)					
			}
			case _ => throw new IllegalArgumentException("Invalid quadrant: "+quadrant)
		}
	}
	
	def setValue(x:Int,y:Int,data:Boolean) 
	{
		require( contains( x , y ) ) // TODO: Performance ?
		
		// find node to set the value on
		var quadrant = 0
		while( quadrant < 4 ) {
			val node = getNode( quadrant );
			if ( node != null && node.contains(x,y) ) {
				node.setValue(x, y, data)
				return;
			} 
			else if ( node == null ) {
				val env  = createEnvelope( quadrant )
				if ( env.contains( x , y ) ) {
					
				}
			}
		}
	}
}