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

import cats.implicits._

import model.{AttributeName, AttributeValue}
import Schema.{num, record, str} // TODO try `Schema => S` rename?

class SchemaSpec extends UnitSpec {
  case class User(id: Int, name: String)
  case class Role(capability: String, user: User)

  // TODO add more parameters to status
  sealed trait Status
  case class Error(message: String) extends Status
  case class Auth(role: Role) extends Status

  sealed trait State
  case object Open extends State
  case object Closed extends State
  case class Door(state: State)

  "schema" should {
    "encode/decode a product" in {
      val user = User(203, "tim")

      val schema = record[User] { field =>
        (
          field("id", num, _.id),
          field("name", str, _.name)
        ).mapN(User.apply)
      }

      val expected = AttributeValue.m(
        AttributeName("id") -> AttributeValue.n(user.id),
        AttributeName("name") -> AttributeValue.s(user.name)
      )

      val encoded = schema.write(user)
      val decoded = schema.read(encoded)

      assert(encoded === expected)
      assert(decoded === user)
    }

    "encode/decode a product using arbitrary field names" in {
      val user = User(203, "tim")

      val schema = record[User] { field =>
        (
          field("number", num, _.id),
          field("label", str, _.name)
        ).mapN(User.apply)
      }

      val expected = AttributeValue.m(
        AttributeName("number") -> AttributeValue.n(user.id),
        AttributeName("label") -> AttributeValue.s(user.name)
      )

      val encoded = schema.write(user)
      val decoded = schema.read(encoded)

      assert(encoded === expected)
      assert(decoded === user)
    }

    "encode/decode a product including additional info" in {
      val user = User(203, "tim")

      val versionedSchema = record[User] { field =>
        field.const("version", str, "1.0") *> (
          field("id", num, _.id),
          field("name", str, _.name)
        ).mapN(User.apply)
      }

      val expected = AttributeValue.m(
        AttributeName("version") -> AttributeValue.s("1.0"),
        AttributeName("id") -> AttributeValue.n(user.id),
        AttributeName("name") -> AttributeValue.s(user.name)
      )

      val encoded = versionedSchema.write(user)
      val decoded = versionedSchema.read(encoded)

      assert(encoded === expected)
      assert(decoded === user)
    }

    "encode/decode a product including additional info, nested" in {
      val user = User(203, "tim")

      val schema = record[User] { field =>
        (
          field("id", num, _.id),
          field("name", str, _.name)
        ).mapN(User.apply)
      }
      val versionedSchema: Schema[User] = Schema.record[User] { field =>
        field.const("version", str, "1.0") *>
          field.id("body", schema)
      }

      val expected = AttributeValue.m(
        AttributeName("version") -> AttributeValue.s("1.0"),
        AttributeName("body") -> AttributeValue.m(
          AttributeName("id") -> AttributeValue.n(user.id),
          AttributeName("name") -> AttributeValue.s(user.name)
        )
      )

      val encoded = versionedSchema.write(user)
      val decoded = versionedSchema.read(encoded)

      assert(encoded === expected)
      assert(decoded === user)
    }

    "encode/decode (nested) ADTs using a discriminator" in {
      val user = User(203, "tim")
      val role = Role("admin", user)
      val error = Error("MyError")
      val auth = Auth(role)

      val userSchema: Schema[User] = record[User] { field =>
        (
          field("id", num, _.id),
          field("name", str, _.name)
        ).mapN(User.apply)
      }
      val roleSchema: Schema[Role] = record[Role] { field =>
        (
          field("capability", str, _.capability),
          field("user", userSchema, _.user)
        ).mapN(Role.apply)
      }
      val statusSchema: Schema[Status] = Schema.oneOf[Status] { alt =>
        alt("error", str)(Error(_)) { case Error(v) => v } |+|
          alt("auth", roleSchema)(Auth(_)) { case Auth(v) => v }
      }

      val expectedError = AttributeValue.m(
        AttributeName("error") -> AttributeValue.s(error.message)
      )
      val expectedAuth = AttributeValue.m(
        AttributeName("auth") -> AttributeValue.m(
          AttributeName("capability") -> AttributeValue.s(role.capability),
          AttributeName("user") -> AttributeValue.m(
            AttributeName("id") -> AttributeValue.n(role.user.id),
            AttributeName("name") -> AttributeValue.s(role.user.name)
          )
        )
      )

      val encodedError = statusSchema.write(error)
      val encodedAuth = statusSchema.write(auth)
      val decodedError: Status = statusSchema.read(encodedError)
      val decodedAuth: Status = statusSchema.read(encodedAuth)

      assert(encodedError === expectedError)
      assert(encodedAuth === expectedAuth)
      assert(decodedError === error)
      assert(decodedAuth === auth)
    }

    "encode/decode (nested) ADTs using an embedded \"type\" field" in {
      val user = User(203, "tim")
      val role = Role("admin", user)
      val error = Error("MyError")
      val auth = Auth(role)

      val userSchema: Schema[User] = record[User] { field =>
        (
          field("id", num, _.id),
          field("name", str, _.name)
        ).mapN(User.apply)
      }
      val roleSchema: Schema[Role] = record[Role] { field =>
        (
          field("capability", str, _.capability),
          field("user", userSchema, _.user)
        ).mapN(Role.apply)
      }
      val statusSchema: Schema[Status] = {
        val errorSchema: Schema[String] = Schema.record[String] { field =>
          field.const("type", str, "error") *>
            field.id("body", str)
        }

        val authSchema: Schema[Role] = Schema.record[Role] { field =>
          field.const("type", str, "auth") *>
            field.id("body", roleSchema)
        }

        Schema.oneOf[Status] { alt =>
          alt.from(errorSchema)(Error(_)) { case Error(v) => v } |+|
            alt.from(authSchema)(Auth(_)) { case Auth(v) => v }
        }
      }

      val expectedError = AttributeValue.m(
        AttributeName("type") -> AttributeValue.s("error"),
        AttributeName("body") -> AttributeValue.s(error.message)
      )
      val expectedAuth = AttributeValue.m(
        AttributeName("type") -> AttributeValue.s("auth"),
        AttributeName("body") -> AttributeValue.m(
          AttributeName("capability") -> AttributeValue.s(role.capability),
          AttributeName("user") -> AttributeValue.m(
            AttributeName("id") -> AttributeValue.n(role.user.id),
            AttributeName("name") -> AttributeValue.s(role.user.name)
          )
        )
      )

      val encodedError = statusSchema.write(error)
      val encodedAuth = statusSchema.write(auth)
      val decodedError: Status = statusSchema.read(encodedError)
      val decodedAuth: Status = statusSchema.read(encodedAuth)

      assert(encodedError === expectedError)
      assert(encodedAuth === expectedAuth)
      assert(decodedError === error)
      assert(decodedAuth === auth)
    }
  }

  // TODO objects both ways, then encode/decode a Sum
  // TODO eliminate combinator and encode manually
  // TODO this uses a discriminator, replicate this test both ways, then nested ADTs
  // or perhaps show ADTs first, then encoding of objects
  "encode/decode objects as empty records" in {
    val openDoor = Door(Open)
    val closedDoor = Door(Closed)

    val stateSchema: Schema[State] = {
      val openSchema: Schema[Open.type] = Schema.record[Open.type] { _ =>
        Schema.structure.Ap.pure(Open)
      }
      val closedSchema: Schema[Closed.type] = Schema.record[Closed.type] { _ =>
        Schema.structure.Ap.pure(Closed)
      }
      Schema.oneOf[State] { alt =>
        alt("open", openSchema)(_ => Open) { case Open => Open } |+|
          alt("closed", closedSchema)(_ => Closed) { case Closed => Closed }
      }
    }
    val doorSchema = record[Door] { field =>
      field("state", stateSchema, _.state).map(Door.apply)
    }

    val expectedOpen = AttributeValue.m(
      AttributeName("state") -> AttributeValue.m(
        AttributeName("open") -> AttributeValue.m()
      )
    )
    val expectedClosed = AttributeValue.m(
      AttributeName("state") -> AttributeValue.m(
        AttributeName("closed") -> AttributeValue.m()
      )
    )

    val encodedOpen = doorSchema.write(openDoor)
    val encodedClosed = doorSchema.write(closedDoor)
    val decodedOpen = doorSchema.read(encodedOpen)
    val decodedClosed = doorSchema.read(encodedClosed)

    assert(encodedOpen === expectedOpen)
    assert(encodedClosed === expectedClosed)
    assert(decodedOpen === openDoor)
    assert(decodedClosed === closedDoor)
  }

  val inferenceCompilationSpec = {
    import Schema._
    val userSchema: Schema[User] = record[User] { field =>
      (
        field("id", num, _.id),
        field("name", str, _.name)
      ).mapN(User.apply)
    }
    val roleSchema: Schema[Role] = record[Role] { field =>
      (
        field("capability", str, _.capability),
        field("user", userSchema, _.user)
      ).mapN(Role.apply)
    }

    // can ascribe manually
    field("capability", str, (_: Role).capability)
    alt("auth", roleSchema)(Auth(_): Status) { case Auth(v) => v }
    alt.from(roleSchema)(Auth(_): Status) { case Auth(v) => v }

    // the library helps you
    field[Role]("capability", str, _.capability)
    alt[Status]("auth", roleSchema)(Auth(_)) { case Auth(v) => v }
    alt[Status].from(roleSchema)(Auth(_)) { case Auth(v) => v }
  }

  implicit class CodecSyntax[A](schema: Schema[A]) {
    def read(v: AttributeValue): A =
      Decoder.fromSchema(schema).read(v).toOption.get

    def write(v: A): AttributeValue =
      Encoder.fromSchema(schema).write(v).toOption.get
  }
}
