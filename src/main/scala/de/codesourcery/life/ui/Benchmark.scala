package de.codesourcery.life.ui

import scala.collection.mutable.ListBuffer
import java.text.DecimalFormat

class Benchmark {

  private val results = new ListBuffer[Double]()
  
  private var startTime : Long = 0
  private var endTime : Long = 0
  private var counter = 0
  var averageCellsPerSecond : Double = 0.0d
  
  def time( func : () => Double ) {
    
    if ( endTime != 0 ) {
      func()
      return
    }
    
    var time1 = System.currentTimeMillis
    if ( startTime == 0 ) {
      startTime = time1;
    }
    val cellCount = func()
    val time2 = System.currentTimeMillis
    val delta = ( time2 - time1 )
    
    counter += 1;
    if ( ( counter % 400 ) == 0 ) {
      val cellsPerMillisecond = ( cellCount / delta.asInstanceOf[Double] )
      val cellsPerSecond = cellsPerMillisecond * 1000.0
      println("Cells/s : "+formatCellsPerSecond( cellsPerSecond ) +"( "+cellCount+" cells in "+delta+" millis)")
      results.append( cellsPerSecond )
    }
  }
  
  def formatCellsPerSecond( value : Double ) : String =  new DecimalFormat("###,###,###,###,###").format( value )
  
  def stop() {
    if ( endTime == 0 ) {
    	endTime = System.currentTimeMillis
    	averageCellsPerSecond = calcAverageCellsPerSecond()
    }
  }
  
  private def calcAverageCellsPerSecond() : Double = {
    if ( startTime == 0 || endTime == 0 || results.size == 0 ) {
      return 0.0d
    }
    
    val total = results.foldLeft(0.0d) { (a,b) => a+b }
    total / results.size
  }
  
  def reset() {
    results.clear()
    startTime = 0
    endTime = 0
    counter = 0
  }
  
}