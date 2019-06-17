/*
 * Copyright 2019 OVO Energy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dynosaur

object derivation {
  /*
derivation experiment:
fully auto
only records
non nested records
only ints and strings as fields
   */
  import dynosaur.codec._
  import Schema.structure.{Field, Ap}
  import shapeless._
  import shapeless.labelled._

  import shapeless.record._
  import shapeless.syntax.singleton._
  import shapeless.ops.record._
  import cats.sequence._

  // trait GetField[R <: HList, A] {
  //   def field: Option[Field[R, A]]
  // }
  // object GetField {
  //   implicit def hnil[A]: GetField[HNil, A]
  // }

  trait GetFields[In] {
    type Out
    def fields: Out
  }
  object GetFields {
    type Aux[I, O] = GetFields[I] { type Out = O }

    implicit def hnil: GetFields.Aux[HNil, HNil] = new GetFields[HNil] {
      type Out = HNil
      def fields = HNil
    }

    implicit def cons[K <: Symbol, V, T <: HList, R <: HList](
        implicit
        key: Witness.Aux[K],
        schema: Schema[V],
        rest: GetFields.Aux[T, R]
    ): GetFields.Aux[FieldType[K, V] :: T, Field[FieldType[K, V] :: T, V] :: R] =
      new GetFields[FieldType[K, V] :: T] {
        type Out = Field[FieldType[K, V] :: T, V] :: R

        def fields =
          Field(
            key.value.name,
            schema,
            (in: FieldType[K, V] :: T) => in.head
          ) :: rest.fields
      }

    implicit def generic[T, Repr <: HList, O <: HList, R](
        implicit gen: LabelledGeneric.Aux[T, Repr],
        getFields: GetFields.Aux[Repr, O],
        builder: Traverser.Aux[O, selectFields.type, Ap[Field[Repr, ?], R]]
    ): GetFields.Aux[T, Ap[Field[Repr, ?], R]] = new GetFields[T] {
      type Out = Ap[Field[Repr, ?], R]

      def fields: Out =
        getFields.fields.traverse(selectFields) // .map(x => gen.from(x))
    }

    object selectFields extends Poly1 {
      implicit def all[R, E]: Case.Aux[Field[R, E], Ap[Field[R, ?], E]] = at {
        f =>
          Ap.lift(f)
      }
    }
  }

  implicit def ints: Schema[Int] = Schema.num
  implicit def strings: Schema[String] = Schema.str

  case class MyC(name: String)

  def foo[T, R <: HList](proxy: T)(
      implicit b: LabelledGeneric.Aux[T, R],
      fields: GetFields[R]
  ) = fields.fields

  val gen = LabelledGeneric[MyC]
  val a = foo(MyC("hello")).head.get(gen.to(MyC("yo")))

  // TODO Inspect this to work on problems
  val b = the[GetFields[MyC]]
}
