package playground

object NaturalNumbers extends App{
  sealed trait Nat{
    type Add[N <: Nat] <: Nat
    type Multiply[N <: Nat] <: Nat
  }

  trait _0 extends Nat {
    type Add[N <: Nat] = N
    type Multiply[N <: Nat] = _0
  }

  trait Inc[N <: Nat] extends Nat {
    type Add[N2 <: Nat] = Inc[N#Add[N2]]
    type Multiply[N2 <: Nat] = N2#Add[N2#Multiply[N]]
  }

  type _1 = Inc[_0]
  type _2 = Inc[_1]
  type _3 = Inc[_2]
  type _4 = Inc[_3]
  type _5 = Inc[_4]
  type _6 = Inc[_5]
  type _7 = Inc[_6]
  type _8 = Inc[_7]
  type _9 = Inc[_8]
  type _10 = Inc[_9]
  type _11 = Inc[_10]

  type +[A <: Nat, B <: Nat] = A#Add[B]
  type *[A <: Nat, B <: Nat] = A#Multiply[B]

  implicitly[_2#Add[_3] =:= _5]
  implicitly[_2#Add[_2] =:= _4]
  implicitly[_2#Multiply[_3] =:= _6]
  implicitly[_2#Multiply[_2] =:= _4]
  implicitly[_2#Multiply[_4] =:= _8]

  implicitly[_4+_3 =:= _7]
  implicitly[_5*_2 =:= _10]

}
