package introduction

object Variance {
  class Animal
  class Dog(val name: String) extends Animal
  trait Vet[-T] {
    def heal(animal: T): Boolean
  }

  val myDog: Dog = Dog("Buddy")
  val myVet: Vet[Dog] = (animal: Animal) => {
    println(s"You will be fine, $animal")
    true
  }
  myVet.heal(myDog)
}

object VariancePosition {
  class Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  class MyList[+T]
  // if Dogs are Animals then is a MyList[Dog] also a MyList[Animal]
  // the variance question

  //  1 - yes => generic type is covariance
  val anAnimal = new Dog
  val animals: MyList[Animal] = new MyList[Dog]

  //  2 - no => generic type is INVARIANT

  //  3 - hell no! backwards => generic type is CONTRAVARIANT
  class Vet[-T]
  val lassiesVet: Vet[Dog] = new Vet[Animal]

  // variance problem
//  class MyList2[+T] {
//    def head: T
//    def tail: MyList2[T]
//    def add(element: T): MyList2[T]
//  }
  /**
   * The types of val fields are in COVARIANT position!
   */
  class Vet2[-T](val favoriteAnimal: T)
  val garfield = new Cat
  val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
  val lassiesVet2: Vet2[Dog] = theVet
  val lassie: Dog = lassiesVet2.favoriteAnimal // type conflict now

  /**
   * The types of val fields are in COVARIANT position!
   */
  class Vet3[-T](var favoriteAnimal: T)

  // types of var fields are ALSO in CONTRAVARIANT position
  class MutableOption[+T](var contents: T)
  val maybeAnimal: MutableOption[Animal] = new MutableOption[Dog](new Dog)
  maybeAnimal.contents = new Cat // type conflict again

  // type of method arguments
  class MyList2[+T] {
    def add(element: T): MyList2[T] = ???
  }

  val animals2: MyList2[Animal] = new MyList2[Cat]
  val moreAnimals2 = animals2.add(new Dog) // type conflict again and again!

  class VetAnother3[-T] {
    def heal(animal: T): Boolean = true
  }

  val lassiesVetAnother3: VetAnother3[Dog] = new VetAnother3[Animal]
  lassiesVetAnother3.heal(new Dog)
//  lassiesVetAnother3.heal(new Cat) // legit error


  /**
   *   method return types are in COVARIANT positions
   */
  abstract class Vet4[-T] {
    def rescueAnimal(): T
  }

  val vet4: Vet4[Dog] = new Vet4[Animal] {
    override def rescueAnimal(): Animal = new Cat
  }
  vet4.rescueAnimal() // Obvious type conflict again!

  /**
   * Here you can think of S being Animal, while T being Cat or Dog
   */
  class MyListC[+T] {
    def add[S >: T](element: S): MyListC[S] = new MyListC[S]
  }

  /**
   * Here you can think of T being Animal, while S being Cat or Dog
   */
  class VetC[-T] {
    def rescueAnimal[S <: T](): S = ???
  }
  val lassiesVetC: VetC[Dog] = new VetC[Animal]
  val rescuedDogC: Dog = lassiesVetC.rescueAnimal() // rescueAnimal[Dog]
}
