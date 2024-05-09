# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

end

class MyBoard < Board
  # your enhancements here
  def rotate_upside_down
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    super
    @board = MyBoard.new(self)
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_upside_down})
  end
end


