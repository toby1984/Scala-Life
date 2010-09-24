package de.codesourcery.life.entities

import scala.xml.Node
import scala.xml.Elem
import java.awt.Rectangle

/**
 * This is the simulation's data model.
 * 
 * <p>
 * This class implements the actual 'Game of Life'
 * algorithm on an (at least mathematically) 
 * infinite 'game board' and also holds the
 * simulation's data model (cell data and
 * a generation counter).</p>
 * 
 * <p>Internally, simulation data gets stored as 
 * boolean values (<code>true</code> = cell is alive)
 * in a two-dimensional array.</p>
 * <p>To simulate an infinite extend of the 
 * 'playing field' , the internal array
 * is treated as a torus (X/Y coordinates
 * get wrapped around when trying 
 * to access elements beyond the actual
 * array width / height).</p>
 *
 * @author tobias.gierke@code-sourcery.de
 */
class Board private (private var torus : Torus[Boolean] ) {
	/**
	 * Holds the simulation's current generation counter.
	 */
	private var currentGeneration = 1
	
	private var neighbourCountMap : Option[Torus[Int]] = None
	
	private var regionLocks : RegionLockSet = createLocks(torus.width,torus.height)
	
	private var partitions : Array[Rectangle] = createPartitions(torus.width,torus.height)
	
	private def createLocks(w:Int,h:Int) = {
		new BoardLockSet( w , h , getPartitionCount(w,h) )
	}
	
	private def createPartitions(w:Int,h:Int) = {
		BoardLockSet.partition( w,h , getPartitionCount(w,h) )
	}
	
	private def getPartitionCount(w:Int,h:Int) = Board.cpuCount * 4
	
	/**
	 * Creates a game board with a given
	 * size.
	 */
	def this( w:Int , h:Int) {
		this( Board.createTorus( w,h) )
	}
	
	/**
	 * Returns the actual extent (width) of the underlying
	 * data model.
	 * 
	 * @return
	 */
	def width : Int = torus.width
	
	/**
	 * Returns the actual extent (height) of the underlying
	 * data model.
	 * 
	 * @return
	 */	
	def height : Int = torus.height
	
	override def toString() : String = "Board[ "+width+" x "+height+" ]"
	
	/**
	 * Returns the simulation's current generation counter.
	 * @return
	 */	
	def generation : Int = currentGeneration
	
	/**
	 * Creates an independent copy
	 * of this simulation state.
	 * 
	 * @return
	 */
	def createCopy() : Board = {
		val result = new Board( torus.createCopy() )
		result.currentGeneration = currentGeneration
		result.neighbourCountMap = neighbourCountMap  match {
			case Some(map) => Some(map.createCopy())
			case None => None
		}
		result
	}
	
	def resize(newWidth:Int,newHeight:Int) {
		require( newWidth > 0 )
		require( newHeight > 0 )
		
		println("Changed board size to "+newWidth+"x"+newHeight+" with "+getPartitionCount(newWidth,newHeight)+" partitions.")
				
		regionLocks = createLocks(newWidth,newHeight)
		partitions = createPartitions(newWidth,newHeight)
		torus.resize( newWidth , newHeight )
		neighbourCountMap  = None // ...needs to be recalculated
	}
	
	def printBoard() {
		visitAll( { (x , y , isSet ) => {
			if ( x == 0 ) {
				println
			}
			if ( torus.get( x,y ) ) {
				print(" X |")
			} else {
				print("   |")
			}
		} } ) 
	}
	
	def printNeighbourCount() {
		getNeighbourCountMap().visitAll( { (x , y , count ) => {
			if ( x == 0 ) {
				println
			}
			print( (count+"  ").padTo(3,' ')+"|" )
		} } ) 		
	}
	
	/**
	 * Populates THIS simulation state from
	 * another.
	 * 
	 * This instance will be come an
	 * independent copy of the source
	 * state.
	 * @param source
	 */
	def valueOf( source : Board) {
		torus = source.torus.createCopy
		neighbourCountMap = source.neighbourCountMap match {
			case Some(map) => Some(map.createCopy())
			case None => None
		}		
		currentGeneration = source.currentGeneration 
	}
	
	/**
	 * Resets the simulation to a state
	 * where all cells are dead.
	 * 
	 * The generation counter is reset to 1 as well.
	 */
	def reset() {
		if ( neighbourCountMap.isDefined ) {
			neighbourCountMap.get.clear()
		}
		visitAll( ( x , y , isSet ) => { torus.set( x,y,false ) } )
		currentGeneration = 1
	}
	
	/**
	 * Accepts a function that gets invoked once
	 * for each X-Y coordinate of the
	 * simulation model data.
	 * 
	 * @param func 
	 */
	def visitAll( func : => (Int,Int,Boolean) => Unit ) {
		torus.visitAll( (x : Int,y : Int,isSet : Boolean) => 
			func( x , y , isSet )
		)
	}
	
	/**
	 * Accepts a function that gets invoked once
	 * for each X-Y coordinate with an alive cell.
	 * 
	 * @param func 
	 */	
	def visitAlive( func : => (Int,Int) => Unit ) {
		torus.visitAlive( (x : Int,y : Int) => 
			func( x , y )
		)
	}	
	
	/**
	 * Returns whether a cell is alive
	 * for a given X-Y coordinate.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	private def get(x:Int,y:Int) : Boolean = torus.get( x, y )
	
	/**
	 * Marks a cell at a given X-Y coordinate as dead.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def clear(x : Int , y : Int) : Board = {
		set(x,y,false)
		this
	}	
	
	private def set(x:Int,y:Int, alive : Boolean ) {
		
		val oldValue =
			torus.set(x,y,alive)
			
		if ( oldValue == alive ) {
			return
		}
		
		// update neighbour count
		// by incrementing or decrementing
		// the count of all direct neighbours
		
		if ( neighbourCountMap.isDefined) 
		{
			val map = neighbourCountMap.get
			val increment = if ( alive ) 1 else -1
			val countFunction : (Int,Int,Boolean) => Unit = (currentX,currentY,isSet) => {
				if ( currentX != x || currentY != y ) {
					val value = map.get( currentX ,currentY )
					var newValue = value + increment
					if ( newValue < 0 ) {
						newValue=0
					}
					if ( value != newValue ) {
						map.set( currentX , currentY , newValue )
					}
				}
			}
			torus.visit( x-1 , y-1 , x+1 , y+1 , countFunction )
		} 
		else {
			println("Full neighbour count scan")
			// calculate neighbour count map 
			// because it's not set yet
			getNeighbourCountMap()
		}
		
		this
	}
	/**
	 * Marks a cell at a given X-Y coordinate as being alive.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def set(x : Int , y : Int) : Board = 
	{
		set(x,y,true)
		this
	}
	
	/**
	 * Returns whether the cell at a given X-Y
	 * coordinate is alive.
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	def isAlive(x:Int,y:Int) : Boolean = torus.get( x , y)
	
	def getNeighbourCountMap() : Torus[Int] = 
	{
		if ( neighbourCountMap.isDefined ) {
			return neighbourCountMap.get
		}
		
		// calculate neighbour count once 
		// for each field and store those
		// in a two-dimensional array
		val neighbourCount = new AnyTorus[Int]( width , height )
		
		val countFunction : (Int ,Int ,Boolean) => Unit = ( x , y , value ) => {
			neighbourCount.set( x , y , getNeighbourCount( x , y ) )
		}
		torus.visitAll( countFunction )
		
		neighbourCountMap = Some( neighbourCount )
		neighbourCount
	}
	
	/**
	 * Returns the number of cells that are
	 * alive around a given X-Y coordinate.
	 * 
	 * <p>
	 * This method counts all cells within
	 * a 3x3 box centered at the given
	 * X-Y coordinate. 
	 * </p>
	 * @param x
	 * @param y
	 * @return
	 */
	private def getNeighbourCount(x:Int,y:Int) : Int = {
		
		var result = 0;
		val f : (Int,Int,Boolean) => Unit = {
			( currentX , currentY , set ) => 
			{
				if ( set && ( currentX != x || currentY != y ) ) {
					result += 1
				}
			}
		}
		
/*       Here we make use of the facts that we 
		
		 - know the iteration order ( row by row, starting
		   with the leftmost array element (zero) on each row)
		
		 - know that invalid array coordinates (less than zero
		   or greater than the actual width/height of
		   the underlying array) automatically get wrapped around
*/		
		torus.visit( x - 1 , y - 1 , x + 1  , y + 1 , f );
		return result
	}	
	/**
	 * Advances the simulation to the next generation.
	 * 
	 * @return <code>true</code> if the new population is different from the previous one
	 */
	def advance() : Boolean = {
		
		val neighbourCount = getNeighbourCountMap()
		
		// apply the rules to a new (initally empty) board
		val newBoard = new Board( Board.createTorus( width,height) )
		
		// set an empty neighbourCount map
		// so calling newBoard.set(x,y,true) will not
		// trigger a full board scan
		newBoard.neighbourCountMap  = Some( new AnyTorus[Int](width,height) )
		
		var isStable = new java.util.concurrent.atomic.AtomicBoolean(true) // set to false if at least one cell dies or comes alive
		
		// hint: this function relies on
		// the fact that the new board is initially
		// clear (=no cells alive) and does
		// not flag 'dead' cells.
		val lifeFunction : ( Int , Int , Boolean) => Unit = (x,y,isAlive) => 
		{
			val count = neighbourCount.get( x ,y )
			
			if ( isAlive && ( count < 2 || count > 3 ) ) {
				// cell dies (and since the new board is initially empty, we don't need to do a thing here)
				isStable.set( false )
			} else if ( ! isAlive && count == 3 ) {
				newBoard.set( x , y , true ) // cell comes to life
				isStable.set( false )
			} else if ( isAlive ) { // cell stays the same
				newBoard.set( x , y , true )
			} // } else { /* cell stays dead */ }
			
		}
		
		val latch = new java.util.concurrent.CountDownLatch( partitions.length )
		
		var index = 0
		while ( index < partitions.length ) {
			Board.queueTask( new CalculationTask( partitions( index ) , latch , lifeFunction ) )
			index += 1
		}
		
		// wait for all partitions to be calculated...
		latch.await()
		
		// advance to next generation
		torus = newBoard.torus
		neighbourCountMap  = newBoard.neighbourCountMap
		
		currentGeneration += 1
		
		return ! isStable.get
	}
	
	class CalculationTask(private val rect:Rectangle,private val latch:java.util.concurrent.CountDownLatch, calcFunction : => ( Int , Int , Boolean) => Unit ) extends Runnable {
		
		def run() 
		{
			try {
				/*
				 * We need to lock a bigger region here
				 * because the calculation always looks
				 * and updates the neighbour count for a 3x3 block around 
				 * a cell.
				 */
				regionLocks.doWithRegionLock( rect.x-1 , rect.y-1 , rect.x + rect.width+1 , rect.y+rect.height+1) {
					torus.visit( rect.x , rect.y , rect.x + rect.width , rect.y+rect.height , calcFunction )
				}
			} catch {
				case ex : Throwable => {
					ex.printStackTrace()
				}
			} finally {
				latch.countDown()
			}
		}
	}
}

object Board {
	
	// order of variables is IMPORTANT here....
	val cpuCount : Int = {
		val result = Runtime.getRuntime.availableProcessors
		println("Using "+result+" CPUs.")
		result
	}
	
	private val threadPool = 
			java.util.concurrent.Executors.newFixedThreadPool( cpuCount )
		
	
	def queueTask( task : Runnable ) {
		threadPool.execute( task )
	}
	
	/**
	 * Factory method.
	 * 
	 * @param width
	 * @param height
	 * @return
	 */
	def apply( width : Int , height : Int ) : Board = new Board( width , height )
	
	/**
	 * Saves simulation state to a XML file.
	 * 
	 * @param board
	 * @param file
	 */
	def saveToFile(board:Board , file : String ) {
		
		var cells : scala.xml.NodeSeq = null
			
		board.visitAll( (x:Int,y:Int,isSet:Boolean) => {
			if ( isSet ) {
				if ( cells == null ) {
					cells = <cell x={x.toString} y={y.toString}/>
				} else {
					cells = cells ++ <cell x={x.toString} y={y.toString}/>
				}
			}
		} )
		
		val node = <life>
                     <width>{board.width}</width>
                     <height>{board.height}</height>
                     <generation>{board.generation}</generation>
                     <cells>{cells}</cells>
                   </life>
		
		scala.xml.XML.saveFull( file , node , "UTF-8" , true , null )
	}
	
	/**
	 * Loads simulation state from a XML file.
	 * 
	 * @param board
	 * @param file
	 */
	def populateFromFile(board:Board , file:String) {
		
		val data = scala.xml.XML.loadFile( file )
		
		val w = ( data \\ "width" ).text.toInt
		val h = ( data \\ "height" ).text.toInt
		val generation = ( data \\ "generation" ).text.toInt
		
		val tmp = new Board( w , h )
		tmp.currentGeneration = generation
		
		val aliveCells = ( data \\ "cell" )
		for ( cell <- aliveCells ) {
			val x = ( cell \ "@x" ).text.toInt
			val y = ( cell \ "@y" ).text.toInt
			tmp.set(x, y)
		}
		board.torus.setData( tmp.torus.getData() )
		board.currentGeneration = tmp.currentGeneration
	}

	def createTorus(w:Int,h:Int) : Torus[Boolean] = new BitfieldTorus(w,h)
		// new HashSetTorus(w,h) 
}
