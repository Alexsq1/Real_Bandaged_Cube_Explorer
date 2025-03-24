

--movableFaces :: BandagedCube -> S.Set Face
--movableFaces bCube = S.difference (S.fromList [R, U, F, L, D, B]) allBlockedFaces
--    where
--        allRestr = restrictions bCube
--        allBlockedFaces = S.unions (S.map (blockFaces) allRestr)
--
----Devuelve las capas que bloquea un bloque (>1 centros)
--blockFaces :: S.Set Int -> S.Set Face
--blockFaces xs
--    | (blockingCenters) = S.map (centerToLayer) centers
--    | otherwise = S.fromList []
--    where
--        centers = S.filter (>= 48) xs --Número de centros de un bloque
--        blockingCenters = (length centers) >= 2 --Bool, si bloquea varios
--
----aux



--centerToLayer :: Int -> Face
--centerToLayer 48 = U
--centerToLayer 49 = F
--centerToLayer 50 = R
--centerToLayer 51 = B
--centerToLayer 52 = L
--centerToLayer 53 = D
--

