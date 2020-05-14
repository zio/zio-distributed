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
 * Map[Key, Map[Key (String), Record (Values)] (Record)]
 *
 *
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
  object TypeTag {

    def apply[A](implicit tagType: TypeTag[A]): TypeTag[A] = tagType

    implicit case object TInt     extends TypeTag[Int]
    implicit case object TString  extends TypeTag[String]
    implicit case object TBoolean extends TypeTag[Boolean]
  }

  sealed case class FieldSchema[S <: Schema](name: String, schema: S)

  sealed trait Schema
  object Schema {

    import RecordSchema._

    def field[S <: Schema](name: String, schema: S): Singleton[S] =
      Cons(FieldSchema(name, schema), Empty)

    val string: PrimitiveSchema[String]   = prim[String]
    val int: PrimitiveSchema[Int]         = prim[Int]
    val boolean: PrimitiveSchema[Boolean] = prim[Boolean]

    def int(name: String): Singleton[PrimitiveSchema[Int]]         = field(name, int)
    def string(name: String): Singleton[PrimitiveSchema[String]]   = field(name, string)
    def boolean(name: String): Singleton[PrimitiveSchema[Boolean]] = field(name, boolean)

    def map[K <: Schema, V <: Schema](keySchema: K, valueSchema: V): MapSchema[K, V] =
      MapSchema(keySchema, valueSchema)

    def prim[A: TypeTag]: PrimitiveSchema[A] = PrimitiveSchema(TypeTag[A])

    sealed case class PrimitiveSchema[A](tag: TypeTag[A])                               extends Schema
    sealed case class MapSchema[K <: Schema, V <: Schema](keySchema: K, valueSchema: V) extends Schema

    sealed trait RecordSchema extends Schema {
      type Append[That <: RecordSchema] <: RecordSchema
      type Repr

      def fields: Repr

      def :*:[That <: RecordSchema](that: That): Append[That]
    }
    object RecordSchema {
      type Empty                  = Empty.type
      type Singleton[A <: Schema] = Cons[A, Empty]

      case object Empty extends RecordSchema {
        type Repr                         = Unit
        type Append[That <: RecordSchema] = That

        def fields: Repr = ()

        def :*:[That <: RecordSchema](that: That): Append[That] = that
      }

      sealed case class Cons[A <: Schema, B <: RecordSchema](field: FieldSchema[A], tail: B) extends RecordSchema {
        type Repr                         = (FieldSchema[A], tail.Repr)
        type Append[That <: RecordSchema] = Cons[A, tail.Append[That]]

        def fields: Repr = (field, tail.fields)

        def :*:[A <: RecordSchema](that: A): Append[A] = Cons(field, that :*: tail)
      }
    }

    object :*: {
      def unapply[A, B](t: (A, B)): Some[(A, B)] =
        Some(t)
    }
  }

  sealed trait DistributedError
  object DistributedError {}

  sealed trait Namespace { self =>
    def name: String

    type Tag

    def structure[A <: Schema](name0: String, schema0: A): Structure.Aux[A, Tag] =
      new Structure[A] {
        def name   = name0
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
    def createStructure[S <: Schema, NS](
      namespace: Namespace.Aux[NS],
      col: Structure.Aux[S, NS]
    ): IO[DistributedError, Unit]

    def structures[A](namespace: Namespace.Aux[A]): IO[DistributedError, Set[Structure.Aux[Schema, A]]]

    def commit[T, V](transaction: DTransaction[T, V]): IO[DistributedError, V]

    def namespaces: IO[DistributedError, Set[Namespace]]
  }
  sealed trait DTransaction[-Source, +Value]
  object DTransaction {
    sealed case class Constant[A: TypeTag](value: A)              extends DTransaction[Any, A]
    sealed case class GetMapValue[K: TypeTag, V: TypeTag](key: K) extends DTransaction[Map[K, V], V]
  }

  object Example {

    import Schema._

    lazy val productValue   = string("name") :*: int("origin")
    lazy val productId      = string
    lazy val productCatalog = map(productId, productValue)

    val name :*: origin :*: _ = productValue.fields

    lazy val jarId      = int("organization") :*: string("name") :*: string("version")
    lazy val jarValue   = int("size")
    lazy val jarCatalog = map(jarId, jarValue)

    // lazy val cluster: Cluster = cluster
    // lazy val schema: Schema = schema
    // lazy val dev = Namespace("dev")
    // // Map[UserId, UserProfile]
    // lazy val users = dev.structure("users", schema)

    // lazy val txn: DTransaction[dev.Tag, Int] = txn

    // val result = cluster.commit(txn)
  }
}
