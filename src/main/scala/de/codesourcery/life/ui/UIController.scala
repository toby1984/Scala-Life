package de.codesourcery.life.ui

import de.codesourcery.life.entities.Board
import de.codesourcery.life.simulator._
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Application controller.
 * 
 *  <p>This class is responsible for:
 *  <ul>
 *    <li>Notifying the view of simulation state changes etc.</li>
 *    <li>Reacting to messages from the view about user interaction/requests</li>
 *    <li>Controlling the actual simulation state</li> 
 *  </ul<
 *   </p>
 * 
 * @author tobias.gierke@code-sourcery.de
 */
abstract class UIController(val model : Board , private val view : View ) {

	private var lastState : Option[Board] = None
	
	private val benchmark = new Benchmark	 
		    
	private val listener = new ClockListener() {
		
		def clockStateChanged(isRunning:Boolean) {
			if ( isRunning == false ) {
				benchmark.stop()
			} else {
			    benchmark.reset()
			}
			view.simulatorStateChanged( isRunning )
		}
	
		def onTick() : Boolean = {
			
			// advance the model , stops
			// clock by returning false
			// if the model population
			// no longer changes

		    var continue = false;
			benchmark.time( () => {
					continue = model.advance()
					model.width * model.height
				}
			)
			
			if ( continue ) {
				view.modelChanged()
				true
			} else {
				false
			}
		}
    }
	
	private var benchmarkThread : Option[BenchmarkThread] = None
	
	private val clock = new Clock( listener )
	
	def benchmarkIsRunning() : Boolean = {
	  this.synchronized{
		  benchmarkThread match {
		    case Some(thread) => thread.isAlive
		    case _ => false
		  }
	  }
	}
	
	def resetButtonClicked() {
		updateBoard {
			initModel( model )
		}
	}
	
	def speedChanged(percent:Int) {
		val delay = Math.round( 1000.0  * ( percent / 100.0 ) )
		clock.tickIntervalMillis = delay.asInstanceOf[Int]
		println("Delay set to "+clock.tickIntervalMillis+" milliseconds.")
		view.simulatorSpeedChanged( delay.asInstanceOf[Int] )
	}
	
	protected def initModel( board : Board)
	
	def clearButtonClicked() {
		updateBoard {
			model.reset()
		}
	}
	
	private def updateBoard( func : => Unit ) {
		stopButtonClicked()
		func
		view.modelChanged()
	}
	
	def getSimulationDelayInMillis() : Int = clock.tickIntervalMillis
	
	def saveStateClicked() {
		stopButtonClicked()
		lastState = Some( model.createCopy() )
		view.savedStateAvailable()
	}
	
	def recallStateClicked() {
		
		lastState match {
			case Some( state ) =>  {
				updateBoard { 
					model.valueOf( state ) 
				}
			}
			case _ =>
		}
	}
	
	def modelResizeRequested(width:Int,height:Int) {
		updateBoard {
			if ( width != model.width || height != model.height ) {
				model.resize( width , height )
				view.modelChanged
			}
		}
	}
	
	def cellClicked(x:Int,y:Int) {
		updateBoard {
			if ( model.isAlive( x , y ) ) {
				model.clear( x , y )
			} else {
				model.set( x , y )
			}
			view.modelChanged()
		}
	}
	
	private def internalStopButtonClicked() 
	{
		clock.stop()
	}	
	
	def stopButtonClicked() 
	{
	    val threadToStop = this.synchronized {
	      benchmarkThread
	    }
	    if ( threadToStop.isDefined ) {
	      threadToStop.get.terminate()
        } else {
    	  internalStopButtonClicked()
        }	    
	}
	
	def saveStateToFileClicked() {
		val fileName = view.querySaveFileName()
		if ( fileName.isDefined ) {
			stopButtonClicked()
			Board.saveToFile( model , fileName.get )
		}
	}
	
	def loadStateFromFileClicked() {
		val fileName = view.queryLoadFileName()
		if ( fileName.isDefined ) {
			updateBoard {
				Board.populateFromFile( model , fileName.get )
			}
		}
	}	
	
	private class BenchmarkThread extends Thread 
	{
	    private val abort = new AtomicBoolean(false)
	    
	    override def run() 
	    {
	        val oldDelay = clock.tickIntervalMillis
	        clock.tickIntervalMillis = 0
	        
	    	try {
	    	  view.benchmarkStarted()
	    	  startButtonClicked()
	    	  var durationInSeconds = 30
	    	  while ( ! abort.get && durationInSeconds > 0 ) 
	    	  {
	    	      if ( ( durationInSeconds % 10 ) == 0 ) {
	    	        println("Benchmark running, "+durationInSeconds+" seconds to go ...")
	    	      }
	    		  java.lang.Thread.sleep( 1000 )
	    		  durationInSeconds -= 1
	    	  }
			}
	        finally 
			{
			  view.benchmarkFinished()
			  internalStopButtonClicked()
			  clock.tickIntervalMillis = oldDelay
			  view.displayBenchmarkResults( benchmark )
			  UIController.this.synchronized {
				  benchmarkThread = None
			  }
			}
	    }	  
	    
	    def terminate() {
	      abort.set( true )
	    }
	}
	
	private def performBenchmark() 
	{
	  benchmarkThread = Some( new BenchmarkThread() )
	  benchmarkThread.get.start()
	}	
	
	def benchmarkButtonClicked() 
	{
	  def startBenchmark() {
	    stopButtonClicked()
	    recallStateClicked()
	    performBenchmark()
	  }
	  
	  this.synchronized 
	  {
	     benchmarkThread match {
	       case Some(thread) if ! thread.isAlive =>  startBenchmark()
	       case None => startBenchmark()
	       case _ =>
	     }
	  }
	}
	
	def startButtonClicked() {
		clock.start()
	}
	
	// constructor code
	speedChanged(50)
	view.setController( this ) // this escapes constructor...
	view.modelChanged()
}