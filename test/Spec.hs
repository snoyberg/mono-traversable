import Conduit

main :: IO ()
main = mapM_ yield [1..10] $$ mapC (+1) =$ (await >>= liftIO . print)