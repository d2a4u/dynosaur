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
package codec

import cats.free.FreeApplicative
import cats.data.Chain

sealed trait Schema[A] {
  override def toString = "Schema(..)"
}
object Schema {
  object structure {
    type Ap[F[_], A] = FreeApplicative[F, A]
    val Ap = FreeApplicative

    case object Num extends Schema[Int]
    case object Str extends Schema[String]
    case class Rec[R](p: Ap[Field[R, ?], R]) extends Schema[R]
    case class Sum[A](alt: Chain[Alt[A]]) extends Schema[A]

    case class Field[R, E](name: String, elemSchema: Schema[E], get: R => E)
    trait Alt[A] {
      type B
      def caseSchema: Schema[B]
      def prism: Prism[A, B]
    }

  }

  import structure._

  def str: Schema[String] = Str
  def num: Schema[Int] = Num

  def fields[R](p: Ap[Field[R, ?], R]): Schema[R] = Rec(p)
  def record[R](b: FieldBuilder[R] => Ap[Field[R, ?], R]): Schema[R] =
    fields(b(field))
  def emptyRecord: Schema[Unit] = record(_.pure(()))

  def tag[A](name: String)(schema: Schema[A]): Schema[A] =
    record { field =>
      field(name, schema, x => x)
    }

  def alternatives[A](cases: Chain[Alt[A]]): Schema[A] =
    Sum(cases)
  def oneOf[A](b: AltBuilder[A] => Chain[Alt[A]]): Schema[A] =
    alternatives(b(alt))

  def field[R] = new FieldBuilder[R]
  def alt[A] = new AltBuilder[A]

  class FieldBuilder[R] {
    def apply[E](
        name: String,
        elemSchema: Schema[E],
        get: R => E
    ): Ap[Field[R, ?], E] =
      Ap.lift(Field(name, elemSchema, get))

    def pure[A](a: A): Ap[Field[R, ?], A] = Ap.pure(a)
  }

  class AltBuilder[A] {
    def apply[B_](
        caseSchema_ : Schema[B_]
    )(implicit prism_ : Prism[A, B_]): Chain[Alt[A]] =
      Chain.one {
        new Alt[A] {
          type B = B_
          def caseSchema = caseSchema_
          def prism = prism_
        }
      }
  }
}
