package mpilquist_training

/**
 * Functional caching with explicit state passing
 */
object SecondExample {

  type User = String

  trait SocialService {
    def getFollowerStatsByUser(u: User, c: Cache): (Cache, FollowerStats)
  }

  case class FollowerStats(nrFollowers: Int,
                           nrFollowing: Int)

  case class Cache(statistics: Map[User, FollowerStats],
                   hits: Int,
                   miss: Int) {

    def get(u: User): Option[FollowerStats] = statistics.get(u)
    def update(u: User, fs: FollowerStats): Cache = this.copy(statistics = statistics + (u -> fs))

  }

  object FakeSocialService extends SocialService {
    override def getFollowerStatsByUser(u: User, c: Cache): (Cache, FollowerStats) = {
      val (c1, fsOption) = checkCache(u, c)
      fsOption match {
        case Some(fs) => (c1, fs)
        case None => retrieve(u, c)
      }
    }

    private def retrieve(u: User, c: Cache): (Cache, FollowerStats) = {
      val followerStats = callWS(u)
      (c.update(u, followerStats), followerStats)
    }

    private def callWS(u: User) = FollowerStats(10, 100)

    private def checkCache(u: User, c: Cache): (Cache, Option[FollowerStats]) = {
      val fsOption = c.get(u)
      val cache = fsOption match {
        case Some(fs) => c.copy(hits = c.hits+1)
        case None => c.copy(miss = c.miss+1)
      }
      (cache, fsOption)
    }

  }

}

object Application extends App {
  import mpilquist_training.SecondExample._
  println("Start...")

  val fss = FakeSocialService
  val cache = Cache(statistics = Map.empty, hits = 0, miss = 0)

  val (cache2, fs1) = fss.getFollowerStatsByUser("Matt", cache)
  val (cache3, fs2) = fss.getFollowerStatsByUser("Matt", cache)
  val (cache4, fs3) = fss.getFollowerStatsByUser("Justin", cache)

  println(cache2)
  println(cache3)
  println(cache4)

  println("End...")
}