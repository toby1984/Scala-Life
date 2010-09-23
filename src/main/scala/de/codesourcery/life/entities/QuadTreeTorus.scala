package de.codesourcery.life.entities

class QuadTreeTorus(w:Int,h:Int) extends Torus( new QuadTree(w,h) ) {

	/**
	 * Creates an independent copy of this
	 * torus.
	 *  
	 * @return
	 */
	def createCopy() : Torus[Boolean] = {
		val result = new QuadTreeTorus(width,height)
		result.setData( getData().createCopy )
		result
	}
}