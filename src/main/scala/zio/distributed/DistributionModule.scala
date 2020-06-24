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

  sealed trait Schema {
    type Repr
  }

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

    sealed case class PrimitiveSchema[A](tag: TypeTag[A]) extends Schema {
      type Repr = A
    }
    sealed case class MapSchema[K <: Schema, V <: Schema](keySchema: K, valueSchema: V) extends Schema {
      type Repr = Map[keySchema.Repr, valueSchema.Repr]
    }

    sealed trait RecordSchema extends Schema { self =>
      type Append[That <: RecordSchema] <: RecordSchema
      type Fields[-Source]

      def :*:[A <: Schema](head: FieldSchema[A]): RecordSchema
      def ++[That <: RecordSchema](that: That): Append[That]

      def fields[Source]: Fields[Repr]
    }
    object RecordSchema {
      type Empty                               = Empty.type
      type :*:[A <: Schema, B <: RecordSchema] = Cons[A, B]
      type Singleton[A <: Schema]              = Cons[A, Empty]

      case object Empty extends RecordSchema {
        type Repr                         = Unit
        type Fields[-_]                   = Unit
        type Append[That <: RecordSchema] = That

        def :*:[A <: Schema](head: FieldSchema[A]): Cons[A, Empty] = Cons(head, Empty)
        def ++[That <: RecordSchema](that: That): Append[That]     = that
        def fields[Source]: Fields[Repr]                           = ()
      }

      sealed case class Cons[A <: Schema, B <: RecordSchema](field: FieldSchema[A], tail: B) extends RecordSchema {
        self =>
        type Repr                         = (field.schema.Repr, tail.Repr)
        type Fields[-Source]              = (DTransaction[Nothing, Source, field.schema.Repr], tail.Fields[Source])
        type Append[That <: RecordSchema] = Cons[A, tail.Append[That]]

        def :*:[C <: Schema](head: FieldSchema[C]): Cons[C, Cons[A, B]] = Cons(head, self)
        def ++[That <: RecordSchema](that: That): Append[That]          = Cons(field, tail ++ that)
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
    val name: String

    type Tag

    def structure[A <: Schema](name0: String, schema0: A): Structure.Aux[A, Tag] =
      new Structure[A] {
        val name: String                    = name0
        val schema: A                       = schema0
        val namespace: Namespace.Aux[NSTag] = self

        type NSTag = self.Tag
      }
  }
  object Namespace {
    type Aux[A] = Namespace { type Tag = A }

    def apply(name0: String): Namespace =
      new Namespace {
        val name = name0
      }
  }

  trait Structure[A <: Schema] { self =>
    val name: String
    val schema: A

    type NSTag
    val namespace: Namespace.Aux[NSTag]

    def access: DTransaction[Nothing, NSTag, schema.Repr] = DTransaction.access[NSTag, A](self)
  }
  object Structure {
    type Aux[A <: Schema, Namespace0] = Structure[A] { type NSTag = Namespace0 }
  }

  trait Cluster {
    def createStructure[S <: Schema, NS](
      namespace: Namespace.Aux[NS],
      col: Structure.Aux[S, NS]
    ): IO[DistributedError, Unit]

    def structures[A](namespace: Namespace.Aux[A]): IO[DistributedError, Set[Structure.Aux[Schema, A]]]

    def commit[T, E, V](transaction: DTransaction[T, E, V]): IO[Either[DistributedError, E], V]

    def namespaces: IO[DistributedError, Set[Namespace]]
  }
  sealed trait DTransaction[+Error, -Source, +Value] { self =>
    def >>>[Error1 >: Error, Out](other: DTransaction[Error1, Value, Out]): DTransaction[Error1, Source, Out] =
      DTransaction.Compose(self, other)
  }
  object DTransaction {
    implicit class MapOps[Error, Source, K, V](dt: DTransaction[Error, Source, Map[K, V]]) {
      def get(key: K): DTransaction[Error, Source, Option[V]] = dt >>> GetMapValue(key)
    }

    implicit class OptionOps[Source, A](dt: DTransaction[Nothing, Source, Option[A]]) {
      def some: DTransaction[Unit, Source, A] = dt >>> ExtractSome[A]()
    }

    sealed case class ExtractField[-Source, +Value]() extends DTransaction[Nothing, Source, Value]

    sealed case class Constant[A: TypeTag](value: A) extends DTransaction[Nothing, Any, A]

    //Structure (belongs to the namespace) -> Schema Representation
    sealed abstract case class AccessStructure[NSTag, S <: Schema, Repr] private[DTransaction] (
      s: Structure.Aux[S, NSTag]
    ) extends DTransaction[Nothing, NSTag, Repr]

    sealed case class GetMapValue[K, V](key: K) extends DTransaction[Nothing, Map[K, V], Option[V]]

    sealed case class ExtractSome[A]() extends DTransaction[Unit, Option[A], A]

    case class Compose[Error, A, B, C](
      l: DTransaction[Error, A, B],
      r: DTransaction[Error, B, C]
    ) extends DTransaction[Error, A, C]

    // accessing a structure gets back a Scala representation of the Schema
    def access[NSTag, S <: Schema](s: Structure.Aux[S, NSTag]): DTransaction[Nothing, NSTag, s.schema.Repr] =
      new AccessStructure[NSTag, S, s.schema.Repr](s) {}
  }

  object Example {

    import Schema._

    val productValue   = string("name") ++ int("origin")
    val productId      = string
    val productCatalog = map(productId, productValue)

    val name :*: origin :*: _ = productValue.fields

    val jarId      = int("organization") ++ string("name") ++ string("version")
    val jarValue   = int("size")
    val jarCatalog = map(jarId, jarValue)

    // lazy val cluster: Cluster = cluster
    val dev                     = Namespace("dev")
    val productCatalogStructure = dev.structure("productCatalog", productCatalog)
    val jarCatalogStructure     = dev.structure("jarCatalog", jarCatalog)

    // Use case: access the name belonging to product ID X
    import DTransaction._

    val transaction = productCatalogStructure.access.get("ProductID_X").some >>> name
  }
}
