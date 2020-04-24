package models

case class Value(value: Int){
  //require( value >= 0 && value < 32768)

  def +(that: Value): Value =
    Value((value + that.value) % 32768)

}
