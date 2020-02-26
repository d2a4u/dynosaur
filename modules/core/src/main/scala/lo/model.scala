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
package lo
package model

import dynosaur.model._
import cats.implicits._
import org.http4s.Response

import scala.reflect.macros.whitebox

case class TableName(value: String)

sealed trait ReturnValues
object ReturnValues {

  /**
    * nothing is returned
    */
  case object None extends ReturnValues

  /**
    * the content of the old item is returned
    */
  case object AllOld extends ReturnValues

  case object UpdatedOld extends ReturnValues
  case object AllNew extends ReturnValues
  case object UpdatedNew extends ReturnValues

}

case class ConditionExpression(value: String)

case class ProjectionExpression(value: String)

case class UpdateExpression(value: String)

// TODO Macro to instantiate it from static string
case class ExpressionAlias private (value: String)

object ExpressionAlias {

  class Macros(val c: whitebox.Context) {
    import c.universe._

    def literal(s: c.Expr[String]): Tree =
      s.tree match {
        case Literal(Constant(s: String)) =>
          ExpressionAlias
            .fromString(s)
            .fold(
              e => c.abort(c.enclosingPosition, e),
              _ =>
                q"_root_.dynosaur.lo.model.ExpressionAlias.unsafeFromString($s)"
            )
        case _ =>
          c.abort(
            c.enclosingPosition,
            s"This method uses a macro to verify that a String literal is a valid ExpressionAlias. Use ExpressionAlias.fromString if you have a dynamic String that you want to parse."
          )
      }
  }

  def apply(s: String): ExpressionAlias = macro ExpressionAlias.Macros.literal

  def fromString(str: String): Either[String, ExpressionAlias] =
    if (str.headOption.contains('#')) {
      new ExpressionAlias(str).asRight
    } else {
      s"Valid expression alias must start with '#'. Invalid placeholder: '$str'".asLeft
    }

  def unsafeFromString(str: String): ExpressionAlias =
    fromString(str).getOrElse(
      throw new IllegalArgumentException(
        s"$str is not a valid expression alias"
      )
    )
}

// TODO Macro to instantiate it from static string
case class ExpressionPlaceholder private (value: String)

object ExpressionPlaceholder {

  class Macros(val c: whitebox.Context) {
    import c.universe._

    def literal(s: c.Expr[String]): Tree =
      s.tree match {
        case Literal(Constant(s: String)) =>
          ExpressionPlaceholder
            .fromString(s)
            .fold(
              e => c.abort(c.enclosingPosition, e),
              _ =>
                q"_root_.dynosaur.lo.model.ExpressionPlaceholder.unsafeFromString($s)"
            )
        case _ =>
          c.abort(
            c.enclosingPosition,
            s"This method uses a macro to verify that a String literal is a valid ExpressionPlaceholder. Use ExpressionPlaceholder.fromString if you have a dynamic String that you want to parse."
          )
      }
  }

  def apply(s: String): ExpressionPlaceholder =
    macro ExpressionPlaceholder.Macros.literal

  def fromString(str: String): Either[String, ExpressionPlaceholder] =
    if (str.headOption.contains(':'))
      new ExpressionPlaceholder(str).asRight
    else
      s"Valid expression placeholdert must start with ':'. Invalid placeholder: '$str'".asLeft

  def unsafeFromString(str: String): ExpressionPlaceholder =
    fromString(str).fold(e => throw new IllegalArgumentException(e), identity)

}

case class PutItemRequest(
    tableName: TableName,
    item: AttributeValue.M,
    conditionExpression: Option[ConditionExpression] = None,
    expressionAttributeNames: Map[ExpressionAlias, AttributeRef] = Map.empty,
    expressionAttributeValues: Map[ExpressionPlaceholder, AttributeValue] =
      Map.empty,
    returnValues: ReturnValues = ReturnValues.None
)

case class PutItemResponse(attributes: Option[AttributeValue.M])

case class GetItemRequest(
    tableName: TableName,
    key: AttributeValue.M,
    consistent: Boolean = false,
    projectionExpression: Option[ProjectionExpression] = None,
    expressionAttributeNames: Map[ExpressionAlias, AttributeRef] = Map.empty
)

case class GetItemResponse(
    item: Option[AttributeValue.M]
)

case class DeleteItemRequest(
    tableName: TableName,
    key: AttributeValue.M,
    conditionExpression: Option[ConditionExpression] = None,
    expressionAttributeNames: Map[ExpressionAlias, AttributeRef] = Map.empty,
    expressionAttributeValues: Map[ExpressionPlaceholder, AttributeValue] =
      Map.empty,
    returnValues: ReturnValues = ReturnValues.None
)

case class DeleteItemResponse(
    attributes: Option[AttributeValue.M]
)

case class UpdateItemRequest(
    tableName: TableName,
    key: AttributeValue.M,
    updateExpression: UpdateExpression,
    expressionAttributeNames: Map[ExpressionAlias, AttributeRef] = Map.empty,
    expressionAttributeValues: Map[ExpressionPlaceholder, AttributeValue] =
      Map.empty,
    conditionExpression: Option[ConditionExpression] = None,
    returnValues: ReturnValues = ReturnValues.None
)

case class UpdateItemResponse(
    attributes: Option[AttributeValue.M]
)

object BatchWriteItemsRequest {
  sealed trait WriteRequest
  case class DeleteRequest(key: AttributeValue.M) extends WriteRequest
  case class PutRequest(item: AttributeValue.M) extends WriteRequest
}

case class BatchWriteItemsRequest(
    requestItems: Map[TableName, List[BatchWriteItemsRequest.WriteRequest]]
)

case class BatchWriteItemsResponse(
    unprocessedItems: Map[TableName, List[
      BatchWriteItemsRequest.WriteRequest
    ]]
)

sealed trait AttributeType

object AttributeType {
  case object S extends AttributeType
  case object N extends AttributeType
  case object B extends AttributeType
  case object BOOL extends AttributeType
  case object SS extends AttributeType
  case object NS extends AttributeType
  case object BS extends AttributeType
}

sealed trait KeyType

object KeyType {
  case object HASH extends KeyType
  case object RANGE extends KeyType
}

sealed trait BillingMode {
  def value: String
}

object BillingMode {
  case class Provisioned(throughput: ProvisionedThroughput)
      extends BillingMode {
    val value = "PROVISIONED"
  }
  case object PayPerRequest extends BillingMode {
    val value = "PAY_PER_REQUEST"
  }
}

case class ProvisionedThroughput(
    readCapacityUnits: Int,
    writeCapacityUnits: Int
)

case class GlobalSecondaryIndex(
    indexName: String,
    keySchema: Map[AttributeName, KeyType],
    projection: Projection,
    provisionedThroughput: ProvisionedThroughput
)

case class LocalSecondaryIndex(
    indexName: String,
    keySchema: Map[AttributeName, KeyType],
    projection: Projection
)

sealed trait ProjectionType {
  def value: String
}

object ProjectionType {
  case object KeyOnly extends ProjectionType {
    val value = "KEYS_ONLY"
  }
  case object Include extends ProjectionType {
    val value = "INCLUDE"
  }
  case object All extends ProjectionType {
    val value = "ALL"
  }
}

case class Projection(
    nonKeyAttributes: Set[AttributeName],
    projectionType: ProjectionType
)

sealed trait SseType
object SseType {
  case object AES256 extends SseType
  case object KMS extends SseType
}

case class SseSpecification(
    enabled: Boolean,
    kmsMasterKeyId: String,
    sseType: SseType
)

sealed trait StreamViewType {
  def value: String
}

object StreamViewType {
  case object KeysOnly extends StreamViewType {
    val value = "KEYS_ONLY"
  }
  case object NewImage extends StreamViewType {
    val value = "NEW_IMAGE"
  }
  case object OldImage extends StreamViewType {
    val value = "OLD_IMAGE"
  }
  case object NewAndOldImages extends StreamViewType {
    val value = "NEW_AND_OLD_IMAGES"
  }
}

case class StreamSpecification(
    enabled: Boolean,
    viewType: StreamViewType
)

sealed trait TableStatus {
  def value: String
}

object TableStatus {
  case object Creating extends TableStatus {
    val value = "CREATING"
  }
  case object Updating extends TableStatus {
    val value = "UPDATING"
  }
  case object Deleting extends TableStatus {
    val value = "DELETING"
  }
  case object Active extends TableStatus {
    val value = "ACTIVE"
  }
  case object InaccessibleEncryptionCredentials extends TableStatus {
    val value = "INACCESSIBLE_ENCRYPTION_CREDENTIALS"
  }
  case object Archiving extends TableStatus {
    val value = "ARCHIVING"
  }
  case object Archived extends TableStatus {
    val value = "ARCHIVED"
  }

  def fromString(str: String): Either[String, TableStatus] =
    str match {
      case Creating.value => Creating.asRight
      case Updating.value => Updating.asRight
      case Deleting.value => Deleting.asRight
      case Active.value => Active.asRight
      case InaccessibleEncryptionCredentials.value =>
        InaccessibleEncryptionCredentials.asRight
      case Archiving.value => Archiving.asRight
      case Archived.value => Archived.asRight
      case s => Left(s"Unknown status $s")
    }
}

case class CreateTableRequest(
    tableName: TableName,
    attributeDefinitions: Map[AttributeName, AttributeType],
    keySchema: Map[AttributeName, KeyType],
    billingMode: BillingMode,
    globalSecondaryIndexes: List[GlobalSecondaryIndex] = List.empty,
    localSecondaryIndexes: List[GlobalSecondaryIndex] = List.empty,
    sseSpecification: Option[SseSpecification] = None,
    streamSpecification: Option[StreamSpecification] = None,
    tags: Map[String, String] = Map.empty
)

case class DescribeTableRequest(
    tableName: TableName
)

case class DeleteTableRequest(
    tableName: TableName
)

// Shared model for response of CreateTableRequest and DescribeTableRequest
case class TableStatusResponse(
    tableArn: String,
    tableId: String,
    tableStatus: TableStatus
)

// TODO Model all the DynamoDb errors
case class DynamoHttpError(code: Int, error: DynamoDbError)
    extends Exception(
      s"code: $code, message: ${error.message}"
    )

object DynamoHttpError {
  def apply[F[_]](resp: Response[F], error: DynamoDbError): DynamoHttpError =
    DynamoHttpError(resp.status.code, error)
}

case class DynamoDbError(message: String, retriable: Boolean = false)
    extends Exception(message)
