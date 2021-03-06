package zio.distributed 

/**
 * This modules regroups all the key concepts representing the model of ZIO-Distributed.
 * 
 * There are four key concepts:
 * - schemas
 * - structures 
 * - namespaces
 * - transactions 
 *
 * A schema represents how data is organized. Think about it like a database schema. Concretely,
 * it is modeled using Records and Maps of Records. A schema on its own is simply a description of the
 * data, not the data itself. 
 * 
 * A structure is the named association of a schema and a namespace (more on this later). It is the concrete
 * instantiation of a schema, just like an SQL table in regards of its schema.
 * 
 * A transaction is a set of operation which must be atomically done. These operations are performed on data
 * defined by the schemas in a defined namespace.
 * 
 * Finally, a namespace represents a well defined and isolated space where transactions can be performed.
 * 
 * TODO: Define the relationship between structures and transactions.
 * 
 **/
trait DistributedModule {
  trait IO[+E, +A]

  sealed trait TypeTag[A]

  sealed case class FieldSchema[A: TypeTag](name: String)

  sealed trait Schema

  sealed case class MapSchema[K <: Schema, V <: Schema](keySchema: K, valueSchema: V) extends Schema

  sealed trait RecordSchema extends Schema {
    type Append[That <: RecordSchema] <: RecordSchema 

    def ~ [That <: RecordSchema](that: That): Append[That]
  }
  object RecordSchema {
    type Empty = Empty.type 

    case object Empty extends RecordSchema {
      type Append[That <: RecordSchema] = That

      def ~ [That <: RecordSchema](that: That): Append[That] = that
    }

    sealed case class Cons[A: TypeTag, B <: RecordSchema](field: FieldSchema[A], tail: B) extends RecordSchema {
      type Append[That <: RecordSchema] = Cons[A, tail.Append[That]]

      def ~ [A <: RecordSchema](that: A): Append[A] = Cons(field, tail ~ that)
    }

    def field[A: TypeTag](name: String): Cons[A, Empty] = Cons(FieldSchema(name), Empty)
  }

  sealed trait DistributedError 
  object DistributedError {

  }

  sealed trait Namespace { self =>
    def name: String 

    type Tag 

    def structure[A <: Schema](name0: String, schema0: A): Structure.Aux[A, Tag] = 
      new Structure[A] {
        def name = name0 
        def schema = schema0

        type Namespace = self.Tag
      }
  }
  object Namespace {
    type Aux[A] = Namespace { type Tag = A }

    def apply(name0: String): Namespace = 
      new Namespace {
        def name = name0
      }
  }

  trait Structure[A <: Schema] {
    def name: String
    def schema: A

    type Namespace 
  }
  object Structure {
    type Aux[A <: Schema, Namespace0] = Structure[A] { type Namespace = Namespace0 }
  }

  trait Cluster {
    def createStructure[S <: Schema, NS](namespace: Namespace.Aux[NS], col: Structure.Aux[S, NS]): IO[DistributedError, Unit]

    def structures[A](namespace: Namespace.Aux[A]): IO[DistributedError, Set[Structure.Aux[Schema, A]]]

    def commit[T, V](transaction: DTransaction[T, V]): IO[DistributedError, V]

    def namespaces: IO[DistributedError, Set[Namespace]]
  }
  sealed trait DTransaction[-Source, +Value]
  object DTransaction {
    sealed case class Constant[A: TypeTag](value: A) extends DTransaction[Any, A]
    sealed case class GetMapValue[K: TypeTag, V: TypeTag](key: K) extends DTransaction[Map[K, V], V]
  }

  object Example {
    lazy val cluster: Cluster = cluster 

    lazy val schema: Schema = schema

    lazy val dev = Namespace("dev")

    // Map[UserId, UserProfile]
    lazy val users = dev.structure("users", schema)

    lazy val txn: DTransaction[dev.Tag, Int] = txn 

    val result = cluster.commit(txn)
  }
}

