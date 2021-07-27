package light

case class Light(private var state: LightState = LightOff,
                 private var brightness: Int = 0){

  def turnOff: Light = {
    state = LightOff
    brightness = if (brightness > 0) brightness - 1 else 0
    this
  }

  def turnOn: Light = {
    state = LightOn
    brightness = brightness + 1
    this
  }

  def toggle: Light = {
    state = state match {
        case LightOn => LightOff
        case LightOff => LightOn
      }
    brightness = brightness + 2
    this
  }

  def brightns = brightness

  def stat = state
}

sealed trait LightState {
  override def equals(obj: Any): Boolean =
    this.getClass == obj.getClass
}

case object LightOn extends LightState {
  override def toString: String = "O"
}

case object LightOff extends LightState {
  override def toString: String = "X"
}

