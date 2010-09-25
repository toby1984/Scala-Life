package de.codesourcery.life.ui

import javax.swing.filechooser.FileFilter
import de.codesourcery.life.entities.Board
import java.awt.font._
import java.awt.FlowLayout
import java.awt.BorderLayout
import java.awt.GridBagLayout
import java.awt.GridBagConstraints
import java.awt.event._
import javax.swing.JFileChooser
import javax.swing.SwingUtilities
import javax.swing.JLabel
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JSlider
import javax.swing.event.{ChangeListener,ChangeEvent}

class LifeFrame extends javax.swing.JFrame("The Scala Game of Life") with View {

	private implicit def function2ActionListener( f : => (ActionEvent) => Unit ) = {
		new ActionListener() {
			def actionPerformed(ev:ActionEvent) {
				f( ev )
			}
		}
	}
	
	private[this] var controller : Option[UIController]=None 
	
	private var DRAW_GRID = true
		
	def setController(controller : UIController ) {
		require( controller != null )
		this.controller = Some(controller)
		updateSimulatorSpeed( controller.getSimulationDelayInMillis )
	}
	
	private val speedDial = new JSlider(0,100,50) {
		addChangeListener( new ChangeListener() { 
			def stateChanged(ev:ChangeEvent) {
				controller.foreach( _.speedChanged( getValue() ) )
			}
		})
	}
	
	private def model : Board = controller.get.model
	
	private val drawPanel = new JPanel() 
	{
		
		private val CANVAS_WIDTH = 320
		private val CANVAS_HEIGHT = 240
		
		private val someMouseListener : java.awt.event.MouseAdapter = new java.awt.event.MouseAdapter() 
		{
			var lastCoords : Option[(Int,Int)] = None
			
			override def mouseReleased(ev : java.awt.event.MouseEvent) {
				lastCoords = None
			}
			
			override def mouseDragged(ev : java.awt.event.MouseEvent) {
				
				val currentCoords = getModelCoordsForPoint( model , ev.getX , ev.getY )
				if ( ! lastCoords.isDefined || lastCoords.get != currentCoords ) {
					controller.get.cellClicked( currentCoords._1 , currentCoords._2 )
					lastCoords = Some( currentCoords )
				}
			}
			
			override def mouseClicked( ev : java.awt.event.MouseEvent ) {
				if ( ev.getClickCount == 1 ) {
					controller match {
						case Some(ctrl) => {
							val coords = getModelCoordsForPoint( model , ev.getX , ev.getY )						
							ctrl.cellClicked(  coords._1 , coords._2 )
						}
						case _ =>
					}
				}
			}
		}		
		
		private def getModelCoordsForPoint( board: Board, x : Int , y : Int ) : (Int,Int) = {
			var maxX  = getWidth
			var maxY = getHeight 
		
			val stepX = maxX.asInstanceOf[Float] / board.width.asInstanceOf[Float] 
		    val stepY = maxY.asInstanceOf[Float]  / board.height.asInstanceOf[Float]  
            ( (x / stepX).asInstanceOf[Int]  , (y / stepY).asInstanceOf[Int]  )
	    }
	
		private def paintFunction( graphics : java.awt.Graphics)
		{
  		    var maxX  = getWidth
		    var maxY = getHeight 
			
			val stepX = maxX.asInstanceOf[Float] / model.width.asInstanceOf[Float] 
			val stepY = maxY.asInstanceOf[Float] / model.height.asInstanceOf[Float] 
			
			val oldColor = graphics.getColor
			graphics.setColor( java.awt.Color.black )
			
			// draw grid
			if ( DRAW_GRID ) {
				var x : Float = 0.0f;
				while( x <= maxX ) {
					graphics.drawLine( x.asInstanceOf[Int] , 0 , x.asInstanceOf[Int]  , maxY )
					x += stepX
				}
				
				var y : Float = 0;
				while( y <= maxY) {
					graphics.drawLine( 0 , y.asInstanceOf[Int] , maxX , y.asInstanceOf[Int] )
					y += stepY
				}
			}
			
			// render board
			val drawFunction : (Int,Int) => Unit = {
				( x , y  ) => {

					val x1 : Float = x * stepX
					val y1 : Float = y * stepY
					
					graphics.fillRect( 
								x1.asInstanceOf[Int] , 
								y1.asInstanceOf[Int],
								stepX.asInstanceOf[Int]+1, 
								stepY.asInstanceOf[Int]+1)
				}
			}
			model.visitAlive( drawFunction )
			
			val text = "Generation: "+model.generation
			val metrics = graphics.getFontMetrics
			val m =
				metrics.getStringBounds( text , 0, text.length() ,  graphics )
			
			graphics.setColor( java.awt.Color.BLACK )
			graphics.fillRect( 50,50,m.getWidth().asInstanceOf[Int],m.getHeight().asInstanceOf[Int])
			graphics.setColor( java.awt.Color.WHITE )
			graphics.drawString( text, 50,50+metrics.getAscent-1 )
			
			graphics.setColor( oldColor )
		}
		
		override def paint( graphics : java.awt.Graphics ) {
			
			super.paint(graphics )
			
			if ( controller.isDefined ) {
				paintFunction( graphics )
			}
		}	
		
		addMouseListener( someMouseListener )
		addMouseMotionListener( someMouseListener )
		
		val mySize = new java.awt.Dimension( CANVAS_WIDTH, CANVAS_HEIGHT ) 
		setPreferredSize( mySize )
		setBorder( javax.swing.BorderFactory.createLineBorder( java.awt.Color.BLACK ))
	}
	
	private def round(f :Float) = Math.round(f )
	
	private[this] val simulatorDelay = new javax.swing.JTextField() {
		setEditable( false )
	}
	
	private[this] val modelSizeChangeListener : java.awt.event.ActionListener = new java.awt.event.ActionListener() {
		
		def actionPerformed(ev:ActionEvent) 
		{
			val source = ev.getSource
			if ( source == modelWidth || source == modelHeight ) {
				
				var w = model.width
				var h = model.height
				
				def reset() {
					w = model.width
					h = model.height
					modelWidth.setText( w.toString )						
					modelHeight.setText( h.toString )
				}
				
				try {
					w = modelWidth.getText.toInt
					h = modelHeight.getText.toInt
					if ( h <= 0 || w <= 0 ) {
						reset()
						return
					}					
				} catch {
					case _ => {
						reset()
						return
					}
				}
				if ( controller.isDefined ) {
					controller.get.modelResizeRequested(w,h)
				}				
			}
		}
	}	
	
	private[this] val modelWidth = new javax.swing.JTextField() {
		addActionListener( modelSizeChangeListener )
	}
	
	private[this] val modelHeight = new javax.swing.JTextField() {
		addActionListener( modelSizeChangeListener )
	}

	private[this] val toggleGridBox : javax.swing.JCheckBox = new javax.swing.JCheckBox() {
		setSelected( DRAW_GRID )
		addActionListener( {
			event:ActionEvent => {
				DRAW_GRID = toggleGridBox.isSelected
				drawPanel.repaint()
			} 
		} )
	}
	
	private[this] val loadFromFileButton = new JButton("Load...")
	private[this] val saveToFileButton = new JButton("Save...")
	
	private[this] val clearButton = new JButton("Clear")
	private[this] val startButton = new JButton("Start")
	private[this] val stopButton = new JButton("Stop") {
		setEnabled( false )
	}
	
	private[this] val saveStateButton = new JButton("Save state")
	private[this] val recallStateButton = new JButton("Recall state") {
		setEnabled( false )
	}
	
	private[this] val resetButton = new JButton("Reset")
	
	def savedStateAvailable() {
		recallStateButton.setEnabled( true )
	}
	
	private def updateSimulatorSpeed( delayInMillis : Int ) {
		simulatorDelay.setText( delayInMillis+" ms" )						
	}
	
	def simulatorSpeedChanged( delayInMillis : Int ) {
		updateSimulatorSpeed( delayInMillis )
	}
	
	def simulatorStateChanged(isRunning:Boolean) {
		clearButton.setEnabled( ! isRunning )
		stopButton.setEnabled( isRunning )
		startButton.setEnabled( ! isRunning )
		controlPanel.repaint()
	}
	
	private def showFileChooser( showSaveDialog : Boolean ) : Option[String] = {
		val chooser = new javax.swing.JFileChooser()
		
		chooser.setFileFilter( new FileFilter() {
			def accept(file:java.io.File) : Boolean = {
				file.isDirectory() || 
				( file.isFile() &&  (
					file.getName().toLowerCase.endsWith(".life.xml") ||
					file.getName().toLowerCase.endsWith(".mcl") 
				) )
			}
			
			def getDescription() : String = "*.life.xml"
		} )
		
		val retVal = if ( showSaveDialog) {
			chooser.showSaveDialog( LifeFrame.this ) 
		} else {
			chooser.showOpenDialog( LifeFrame.this ) 
		}
		
		if ( retVal == JFileChooser.APPROVE_OPTION) {
			val path = chooser.getSelectedFile().getAbsolutePath().toLowerCase
			if ( path.endsWith(".life.xml" ) || path.endsWith(".mcl" ) ) {
				Some( path )
			} else {
				Some( path+".mcl" )
			}
		} else {
			None
		}
	}
	
	private class ConstraintBuilder(private val x:Int, private val y:Int) {
		
		private val cnstrs = new GridBagConstraints() 

		cnstrs.gridx = x
		cnstrs.gridy = y
		cnstrs.fill = GridBagConstraints.BOTH
		
		cnstrs.anchor = GridBagConstraints.CENTER
		cnstrs.insets = new java.awt.Insets(5,5,5,5) // top-left-bottom-right
		
		cnstrs.ipadx = 0
		cnstrs.ipady = 0
		cnstrs.weightx = 0.5
		cnstrs.weighty = 0.5
		
		cnstrs.gridheight = 1
		cnstrs.gridwidth = 1	
	
		def build() = cnstrs;
		
		def onlyYInsets() = {
			cnstrs.insets = new java.awt.Insets(5,0,5,0) // top-left-bottom-right	
			this
		}
		
		def anchorLeft() = {
			cnstrs.anchor = GridBagConstraints.WEST
			this
		}
		
		def anchorRight() = {
			cnstrs.anchor = GridBagConstraints.EAST
			this
		}
		
		def noInsets() = {
			cnstrs.insets = new java.awt.Insets(0,0,0,0)
			this
		}
		def noFill() = {
			cnstrs.fill = GridBagConstraints.NONE
			this
		}
		
		def relWidth(relWidth:Double) = {
			cnstrs.weightx=relWidth
			this
		}
		
		def noResizing() = {
			cnstrs.weightx = 0.0
			cnstrs.weighty = 0.0
			this
		}
		
		def width(width:Int) = {
			cnstrs.gridwidth = width
			this
		}
		
		def fillVertical() = {
			cnstrs.fill = GridBagConstraints.VERTICAL
			this
		}		
		
		def fillHorizontal() = {
			cnstrs.fill = GridBagConstraints.HORIZONTAL
			this
		}
		
	}
	
	private def constraints(x:Int,y:Int) : ConstraintBuilder = new ConstraintBuilder(x,y)
	
	def querySaveFileName() : Option[String] = showFileChooser( true )
		
	def queryLoadFileName() : Option[String] = showFileChooser( false )
	
	private val controlPanel = new JPanel() {
		
		private val listener = new ActionListener() {
			
			def actionPerformed(ev:ActionEvent) {
				
				if ( ! controller.isDefined ) 
				{
					return
				}
				
				val button = ev.getSource().asInstanceOf[JButton]
				if ( button == startButton ) {
					controller.get.startButtonClicked()
				} else if ( button == stopButton ) {
					controller.get.stopButtonClicked()
				} else if ( button == clearButton ) {
					controller.get.clearButtonClicked()
				} else if ( button == resetButton ) {
					controller.get.resetButtonClicked()
				} else if ( button == saveStateButton ) {
					controller.get.saveStateClicked()
				} else if ( button == recallStateButton ) {
					controller.get.recallStateClicked()
				} else if ( button == loadFromFileButton) {
					controller.get.loadStateFromFileClicked()
				} else if ( button == saveToFileButton) {
					controller.get.saveStateToFileClicked()
				}				
			}
		}
		
		startButton.addActionListener( listener )
		stopButton.addActionListener( listener )
		clearButton.addActionListener( listener )
		resetButton.addActionListener( listener )
		saveStateButton.addActionListener( listener )
		recallStateButton.addActionListener( listener )		
		loadFromFileButton.addActionListener( listener )
		saveToFileButton.addActionListener( listener )

		// left panel
		val leftPanel = new JPanel()
		leftPanel.setLayout( new GridBagLayout )
		
		leftPanel.add( new JLabel("Draw grid ?" ) , constraints(0,1).fillHorizontal().build() ) 
		leftPanel.add( new JLabel("Width") , constraints(0,2).fillHorizontal().build() )
		leftPanel.add( modelWidth , constraints(0,3).fillHorizontal().build() )
		
		leftPanel.add( startButton , constraints(0,4).fillHorizontal().build() )
		leftPanel.add( clearButton , constraints(0,5).fillHorizontal().build() )
		leftPanel.add( recallStateButton , constraints(0,6).fillHorizontal().build() )
		leftPanel.add( loadFromFileButton , constraints(0,7).fillHorizontal().build() )
		
		// right panel
		val rightPanel = new JPanel()
		rightPanel.setLayout( new GridBagLayout )
		
		rightPanel.add( toggleGridBox , constraints(0,1).fillHorizontal().build() ) 		
		rightPanel.add( new JLabel("Height") , constraints(0,2).fillHorizontal().build() )
		rightPanel.add( modelHeight, constraints(0,3).fillHorizontal().build() )
		
		rightPanel.add( stopButton , constraints(0,4).fillHorizontal().build() )
		rightPanel.add( resetButton , constraints(0,5).fillHorizontal().build() )
		rightPanel.add( saveStateButton , constraints(0,6).fillHorizontal().build() )
		rightPanel.add( saveToFileButton , constraints(0,7).fillHorizontal().build() )
		
		val subPanel = new JPanel()
		subPanel.setLayout( new GridBagLayout )
		subPanel.add( leftPanel, constraints(0,0).build() )
		subPanel.add( rightPanel, constraints(1,0).build() )
		
		setLayout(new GridBagLayout() )
		
		val topPanel = new JPanel()
		topPanel.setLayout(new GridBagLayout() )
		
		topPanel.add( new javax.swing.JLabel( "Simulation delay" ) , constraints(0,0).anchorLeft().noFill().build() )
		topPanel.add( simulatorDelay , constraints( 1, 0 ).anchorRight().fillHorizontal().build() )
		topPanel.add( speedDial , constraints(0,1).fillHorizontal().width(2).build() )
		
		add( topPanel , constraints(0,1).noFill.noResizing.build() )
		add( subPanel , constraints(0,2).width(2).noResizing.noFill.build() )
	}
	
	def modelChanged() {
		modelWidth.setText( model.width.toString )
		modelHeight.setText( model.height.toString )
		 drawPanel.repaint()
	}
	
	setDefaultCloseOperation(  javax.swing.JFrame.EXIT_ON_CLOSE )
	
	private[this] val wrapper = new JPanel
	wrapper.setLayout( new GridBagLayout() )
	
	wrapper.add( controlPanel , constraints(0,0).relWidth(0.1).build() )
	wrapper.add( drawPanel , constraints(1,0).relWidth(0.9).build() )
	
	getContentPane().setLayout( new GridBagLayout() )
	getContentPane().add( wrapper , constraints(0,0).build() )
	pack()
}