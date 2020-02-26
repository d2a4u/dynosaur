package dynosaur

import cats.effect.{ContextShift, IO, Timer}
import java.time.Instant

import scala.concurrent.duration._
import com.ovoenergy.comms.aws.common.CredentialsProvider
import com.ovoenergy.comms.aws.common.model._
import model.{AttributeName, AttributeRef, AttributeValue}
import lo.DynamoDb
import lo.model._
import org.scalatest.BeforeAndAfterAll

class DynamoDbSpec extends IntegrationSpec with BeforeAndAfterAll {

  implicit val patience: PatienceConfig = PatienceConfig(scaled(30.seconds), 500.millis)
  implicit val ctx: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)

  val tableName = TableName(s"dynosaur-it-test-${Instant.now.toEpochMilli}")

  override def beforeAll(): Unit = {
    val active = withDynamoDb { dynamoDb =>
      val creation = dynamoDb.createTable(
        CreateTableRequest(
          tableName,
          Map(AttributeName("id") -> AttributeType.S),
          Map(AttributeName("id") -> KeyType.HASH),
          BillingMode.PayPerRequest
        )
      )
      val describe =
        fs2.Stream
          .repeatEval(dynamoDb.describeTable(DescribeTableRequest(tableName)))
          .metered(1.second)
          .takeWhile(_.tableStatus == TableStatus.Active)
          .handleErrorWith(_ => fs2.Stream(TableStatusResponse("", "", TableStatus.Creating)))
          .interruptAfter(60.seconds)
          .compile
          .drain

      for {
        _ <- creation
        _ <- describe
      } yield ()
    }
    active.unsafeRunSync()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    val deletion = withDynamoDb { dynamoDb =>
       dynamoDb.deleteTable(DeleteTableRequest(tableName))
    }
    deletion.unsafeRunSync()
    super.afterAll()
  }

  "DynamoDb" should {
    "write an item" in {
      withDynamoDb { dynamoDb =>
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          response <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = AttributeValue.M(Map(
              AttributeName("id")->AttributeValue.S("test-1"),
              AttributeName("date")->AttributeValue.N(now.toString)   
            ))
          ))
        } yield response
      }.futureValue shouldBe PutItemResponse(None)
    } 

    "write an item and return overwritten one" in {
      withDynamoDb { dynamoDb =>
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          oldItem = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString),
            AttributeName("value")->AttributeValue.S("I am the old one")
          ))
          newItem = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString),
            AttributeName("value")->AttributeValue.S("I am the new one")
          ))
          _ <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = oldItem
          ))
          response <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = newItem,
            returnValues = ReturnValues.AllOld
          ))
        } yield response.attributes shouldBe Some(oldItem)
      }.futureValue
    }
    
    "get an item" in {
      withDynamoDb { dynamoDb =>

        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          item = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString)   
          ))
          _ <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = item
          ))
          response <- dynamoDb.getItem(GetItemRequest(
            tableName = tableName, 
            key = AttributeValue.M(Map(
              AttributeName("id")->AttributeValue.S("test-1"),
              AttributeName("date")->AttributeValue.N(now.toString)   
            )),
            consistent = true
          ))
        } yield response shouldBe GetItemResponse(Some(item))
      }.futureValue
    } 

    "delete an item" in {
      withDynamoDb { dynamoDb =>

        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          item = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString)   
          ))
          _ <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = item
          ))
          _ <- dynamoDb.deleteItem(DeleteItemRequest(
            tableName = tableName, 
            key = item
          ))
          response <- dynamoDb.getItem(GetItemRequest(
            tableName = tableName, 
            key = AttributeValue.M(Map(
              AttributeName("id")->AttributeValue.S("test-1"),
              AttributeName("date")->AttributeValue.N(now.toString)   
            )),
            consistent = true
          ))
        } yield response shouldBe GetItemResponse(None)
      }.futureValue
    } 

    "update an item" in {
      withDynamoDb { dynamoDb =>

        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          key = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString)   
          ))
          item = AttributeValue.M(key.values ++ Map(
            AttributeName("value")->AttributeValue.S("1")  
          ))
          _ <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = item
          ))
          _ <- dynamoDb.updateItem(UpdateItemRequest(
            tableName = tableName, 
            key = key,
            updateExpression = UpdateExpression("SET #v = :newValue"),
            expressionAttributeNames = Map(
              ExpressionAlias("#v") -> AttributeRef(AttributeName("value"))
            ),
            expressionAttributeValues = Map(
              ExpressionPlaceholder(":newValue") -> AttributeValue.S("2")
            )
          ))
          response <- dynamoDb.getItem(GetItemRequest(
            tableName = tableName, 
            key = key,
            consistent = true
          ))
        } yield response shouldBe GetItemResponse(Some(AttributeValue.M(key.values ++ Map(
          AttributeName("value")->AttributeValue.S("2")  
        ))))
      }.futureValue
    } 

    "write multiple items" in {
      withDynamoDb { dynamoDb =>
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          response <- dynamoDb.batchWriteItems(
            BatchWriteItemsRequest(
              requestItems = Map(
                tableName -> List(
                  BatchWriteItemsRequest.PutRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-1"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                  BatchWriteItemsRequest.PutRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-2"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                )
              )
            )
          )
        } yield response
      }.futureValue shouldBe BatchWriteItemsResponse(Map.empty)
    } 

    "write and delete multiple items" in {
      withDynamoDb { dynamoDb =>
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          key = AttributeValue.M(Map(
            AttributeName("id")->AttributeValue.S("test-1"),
            AttributeName("date")->AttributeValue.N(now.toString)   
          ))
          item = AttributeValue.M(key.values ++ Map(
            AttributeName("value")->AttributeValue.S("1")  
          ))
          _ <- dynamoDb.putItem(PutItemRequest(
            tableName = tableName, 
            item = item
          ))
          response <- dynamoDb.batchWriteItems(
            BatchWriteItemsRequest(
              requestItems = Map(
                tableName -> List(
                  BatchWriteItemsRequest.DeleteRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-1"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                  BatchWriteItemsRequest.PutRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-2"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                )
              )
            )
          )
        } yield response
      }.futureValue shouldBe BatchWriteItemsResponse(Map.empty)
    } 

    "delete multiple items" in {
      withDynamoDb { dynamoDb =>
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          response <- dynamoDb.batchWriteItems(
            BatchWriteItemsRequest(
              requestItems = Map(
                tableName -> List(
                  BatchWriteItemsRequest.DeleteRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-1"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                  BatchWriteItemsRequest.DeleteRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S("test-2"),
                        AttributeName("date")->AttributeValue.N(now.toString)   
                      )
                    )
                  ),
                )
              )
            )
          )
        } yield response
      }.futureValue shouldBe BatchWriteItemsResponse(Map.empty)
    } 

    "return unprocessed items" in {
      withDynamoDb { dynamoDb =>

        // 25 items is the max batch size
        val ids = (0 to 24).map(idx => s"test-$idx")
        
        // 380K string
        val longString = (0 to 380 * 1024).map(_ => 65 + (math.random * 25).toInt).map(_.toChar).mkString
        for {
          now <- IO(Instant.now).map(_.toEpochMilli)
          response <- dynamoDb.batchWriteItems(
            BatchWriteItemsRequest(
              requestItems = Map(
                tableName -> ids.map(id => 
                  BatchWriteItemsRequest.PutRequest(
                    AttributeValue.M(
                      Map(
                        AttributeName("id")->AttributeValue.S(id),
                        AttributeName("date")->AttributeValue.N(now.toString),
                        AttributeName("data")->AttributeValue.s(longString)   
                      )
                    )
                  )
                ).toList
              )
            )
          )
        } yield response.unprocessedItems(tableName)
      }.futureValue should not be empty
    } 
  }

  def withDynamoDb[A](f: DynamoDb[IO] => IO[A]): IO[A] = {
    DynamoDb.resource(CredentialsProvider.default[IO], Region.`eu-west-1`).use(f)
  }

}
