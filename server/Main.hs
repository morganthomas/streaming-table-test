module Server where


import Data.Functor.Of
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant hiding (Stream)
import Servant.Types.SourceT (SourceT (..), StepT (..))
import Streaming (Stream)
import qualified Streaming
import qualified Streaming.Prelude as Streaming
import Test.QuickCheck

import Types


numPeople :: Int
numPeople = 10000


arbitraryPerson :: IO Person
arbitraryPerson = generate arbitrary


peopleStream :: Stream (Of Person) IO ()
peopleStream = Streaming.replicateM numPeople arbitraryPerson


streamSource :: Stream (Of a) IO () -> SourceIO a
streamSource s = SourceT $ \f -> f (Streaming.destroy s (\(x :> xs) -> Yield x xs) Effect (const Stop))


streamingPeople :: Handler (SourceIO Person)
streamingPeople = return $ streamSource peopleStream


server :: Server Api
server = streamingPeople


api :: Proxy Api
api = Proxy


main :: IO ()
main = run 8081 . serve api $ server
