//> using dep se.lth.cs::introprog:1.4.0
package blockbattle
import introprog.* 
import java.awt.Color as JColor

case class Pos(x: Int, y: Int):
    def moved(delta: (Int, Int)): Pos = Pos(x + delta._1, y + delta._2)

case class KeyControl(left: String, right: String, up: String, down: String):
    def direction(key: String): (Int, Int) = {
        var x = 0
        var y = 0
        if key == left then x -= 1
        else if key == right then x += 1
        else if key == up then y -= 1
        else if key == down then y += 1   
        (x, y)
    }

    def has(key: String): Boolean = {
        var a = false 
        if key == left then a = true
        else if key == right then a = true
        else if key == up then a = true
        else if key == down then  a = true
        a
    }

class Mole(val name: String, var pos: Pos, var dir: (Int, Int), val color: java.awt.Color, val keyControl: KeyControl) {

    var points = 0

    override def toString = s"Mole[name=$name, pos=$pos, dir=$dir, points=$points]" 
    
    /** Om keyControl.has(key) så uppdateras riktningen dir enligt keyControl */
    def setDir(key: String): Unit = {
        if keyControl.has(key) == true then dir = keyControl.direction(key)
    } 
    
    /** Uppdaterar dir till motsatta riktningen. */
    def reverseDir(): Unit = {
         dir = (0 - dir._1, 0 - dir._2)
    }

    /** Uppdaterar pos så att den blir nextPos */
    def move(): Unit = pos = nextPos 
    
    /** Ger nästa position enligt riktningen dir utan att uppdatera pos */
    def nextPos: Pos = Pos(pos.x + dir._1, pos.y + dir._2)
}

class BlockWindow(val nbrOfBlocks: (Int, Int), val title: String = "BLOCK WINDOW", val blockSize: Int = 14):
    import introprog.PixelWindow
    val pixelWindow = new PixelWindow(nbrOfBlocks._1 * blockSize, nbrOfBlocks._2 * blockSize, title)

    def setBlock(pos: Pos, color: java.awt.Color): Unit = {
        pixelWindow.fill(pos._1, pos._2, blockSize, blockSize, color)
    }

    def getBlock(pos: Pos): java.awt.Color = pixelWindow.getPixel(pos._1, pos._2)

    def write(text: String, pos: Pos, color: java.awt.Color, textSize: Int = blockSize): Unit = 
    pixelWindow.drawText(text, pos.x * blockSize, pos.y * blockSize, color, textSize)

    def nextEvent(maxWaitMillis: Int = 10): BlockWindow.Event.EventType =
        import BlockWindow.Event._
        pixelWindow.awaitEvent(maxWaitMillis)
        pixelWindow.lastEventType match
        case PixelWindow.Event.KeyPressed => KeyPressed(pixelWindow.lastKey)
        case PixelWindow.Event.WindowClosed => WindowClosed
        case _ => Undefined
     
object BlockWindow:
    def delay(millis: Int): Unit = Thread.sleep(millis)
    
    object Event:
        trait EventType
        case class KeyPressed(key: String) extends EventType
        case object WindowClosed extends EventType
        case object Undefined extends EventType