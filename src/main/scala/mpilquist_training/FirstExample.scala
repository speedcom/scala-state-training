package mpilquist_training


/**
 * Simple Domain show-case
 */
object FirstExample {

  type User = String

  trait SocialService {
    def getFollowerStatsByUser(u: User): FollowerStats
  }

  case class FollowerStats(nrFollowers: Int, nrFollowing: Int)

  case class Cache(
    statistics: Map[User, FollowerStats],
    hits: Int,
    miss: Int) {

    def get(u: User): Option[FollowerStats] = statistics.get(u)

    def update(u: User, fs: FollowerStats): Cache = copy(statistics = statistics + (u -> fs))

  }

}

object App extends App {
  import mpilquist_training.FirstExample._

  object FakeSocialService extends SocialService {
    override def getFollowerStatsByUser(u: User): FollowerStats = FollowerStats(0,0)
  }

}
