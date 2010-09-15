package de.codesourcery.life.entities

class Board(val width:Int , val height:Int)  {

	var board = TwoDimensionalArray[Boolean](width,height)
	
	private var generation = 1
	
	def this(b : Board) = {
		this( b.width , b.height )
		for ( x <- 0 to width ; y <- 0 to height ) {
			if ( b.get( x , y ) ) {
				set( x, y )
			}
		}
	}
	
	override def toString() : String = "Board[ "+width+" x "+height+" ]"
	
	def getGeneration() : Int = {
		return generation
	}
	
	def reset() {
		visitAll( ( x , y , isSet ) => { board.set( x,y,false ) } )
		generation = 1
	}
	
	def printBoard() {
		
		var lastY : Int = -1
		board.visitAll( ( x , y , set ) => {
			if ( y != lastY && lastY > -1 ) {
				println
			}
			lastY = y
			set match {
				case Some( value ) => if ( value ) {
					print( " X | ")
				} else {
					print( "   | " )
				}
				case None => print( "   | " )
			}
		})
	}
	
	def visitAll( func : => (Int,Int,Boolean) => Unit ) {
		board.visitAll( (x : Int,y : Int,isSet : Option[Boolean]) => 
			if ( isSet.isDefined ) {
				func( x , y , isSet.get )
			} else {
				func(x , y, false )
			}
		)
	}
	
	def get(x:Int,y:Int) : Boolean = {
		board.get( x, y ) match {
			case None => false
			case Some(value) => value
		}
	}
	
	def clear(x : Int , y : Int) : Board = {
		board.set(x,y,false)
		this
	}	
	
	def set(x : Int , y : Int) : Board = {
		board.set(x,y,true)
		this
	}
	
	def printNeightbours() {
		
		var lastY : Int = -1
		board.visitAll( ( x , y , set ) => {
			if ( y != lastY && lastY > -1 ) {
				println
			}
			lastY = y
			print( getNeighbourCount(  x , y ) +" | " )
		} )
	}
	
	def isAlive(x:Int,y:Int) : Boolean = board.get( x , y) match {
			case Some(x) => x
			case _ => false
	}
	
	/**
	 * 
	 * @return <code>true</code> if new generation is different from the current one
	 */
	def advance() : Boolean = {
		
		// calculate neighbour count once 
		// for each field
		val neighbourCount = new TwoDimensionalArray[Int]( width , height )
		
		val countFunction : (Int ,Int ,Option[Boolean]) => Unit = ( x , y , value ) => {
			neighbourCount.set( x , y , getNeighbourCount( x , y ) )
		}
		board.visitAll( countFunction )
		
		// apply the rules to a new board
		val newBoard = new TwoDimensionalArray[Boolean](width,height)
		
		var isStable = true 
		
		val lifeFunction : ( Int , Int , Option[Boolean]) => Unit = (x,y,isSet) => 
		{
			val count = neighbourCount.get( x ,y ).get
			var isAlive = if ( isSet.isDefined ) isSet.get else false
			
			if ( isAlive && ( count < 2 || count > 3 ) ) {
				newBoard.set( x , y , false ) // cell dies
				isStable = false
			} else if ( ! isAlive && count == 3 ) {
				newBoard.set( x , y , true ) // cell comes alive
				isStable = false
			} else {
				// just copy state
				newBoard.set( x , y , isAlive )
			}
			
		}
		
		board.visitAll( lifeFunction )
		
		// advance by one generation
		board = newBoard
		generation += 1
		
		return ! isStable
	}

	def getNeighbourCount(x:Int,y:Int) : Int = {
		
		var result = 0;
		val f : (Int,Int,Option[Boolean]) => Unit = {
			( currentX , currentY , isSet ) => 
			{
				val set = if ( isSet.isDefined ) isSet.get else false  
				if ( set && ( currentX != x || currentY != y ) ) {
					result += 1
				}
			}
		}
		
		board.visit( x - 1 , y - 1 , x + 1  , y + 1 , f );
		return result
	}
	
}

object Board {
	
	def apply( width : Int , height : Int ) : Board = new Board( width , height )
	
}
