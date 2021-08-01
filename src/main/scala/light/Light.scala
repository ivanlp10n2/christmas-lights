package light

case class Light(state: LightState = LightOff,
                 brightness: Int = 0){
  def turnOff: Light = this.copy(
    state = LightOff,
    brightness = if (brightness > 0) brightness - 1 else 0
  )

  def turnOn: Light = this.copy(
    state = LightOn,
    brightness = brightness + 1
  )

  def toggle: Light = this.copy(
    state = state match {
        case LightOn => LightOff
        case LightOff => LightOn
      },
    brightness = brightness + 2
  )

  override def toString: String = state.toString
}

sealed trait LightState
case object LightOn extends LightState {
  override def toString: String = "O"
}
case object LightOff extends LightState {
  override def toString: String = "X"
}

sealed trait Operation
case object TurnOn extends Operation
case object TurnOff extends Operation
case object Toggle extends Operation
