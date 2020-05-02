:- module(ai,[bestmove/6])
/** <module> AI
Zoekt de beste mogelijke zet voor een gegeven bord

@author Tibo Vanheule

*/
score(A) :-
            if let retrievedScore = transpositionTable[board] as Double! {
                return retrievedScore
            } else {
                let score = board.getHeuristicScore()
                transpositionTable[board] = score
                return score
            }
        }


board_is_full(Board) :-

/**
 * bestmove(-Arg:board,-Arg:int,-Arg:Alpha,-Arg:Beta).
 *
 * Zoekt de best mogelijke move,
 * @Arg Board spelbord
 * @Arg player welke speler aan zet max of min
 * @Arg Depth maximum diepte van de spelboom
 * @Arg Alpha Alpha max
 * @Arg Delta Delta min
 * @Arg S scrore
 */
bestmove(_,Depth,_,_,_)
bestmove(Board,_,_,_,S,_) :- board_is_full(Board),!,score(S).
bestmove(_,0,_,_,S,_) :- !, score(S).
bestmove(Board,Depth,Alpha,Beta,Player) :-

var bestValue = -Double.infinity
                for move in moves {
                    let updatedBoard = updateBoardWithMove(board: board, move: Hex(position: move, value: .player))
                    bestValue = max(bestValue, alphaBetaPrunedMiniMax(board: updatedBoard, maximizingPlayer: false, depth: depth-1, alpha: alpha, beta: beta))
                    alpha = max(alpha, bestValue)
                    if beta <= alpha {
                        break
                    }
                }
                return bestValue
