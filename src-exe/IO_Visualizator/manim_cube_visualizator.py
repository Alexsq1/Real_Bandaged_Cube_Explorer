import sys
from manim import *
from manim_rubikscube import *

#               |************|
#               |*U1**U2**U3*|
#               |************|
#               |*U4**U5**U6*|
#               |************|
#               |*U7**U8**U9*|
#               |************|
#  |************|************|************|************|
#  |*L1**L2**L3*|*F1**F2**F3*|*R1**R2**R3*|*B1**B2**B3*|
#  |************|************|************|************|
#  |*L4**L5**L6*|*F4**F5**F6*|*R4**R5**R6*|*B4**B5**B6*|
#  |************|************|************|************|
#  |*L7**L8**L9*|*F7**F8**F9*|*R7**R8**R9*|*B7**B8**B9*|
#  |************|************|************|************|
#               |************|
#               |*D1**D2**D3*|
#               |************|
#               |*D4**D5**D6*|
#               |************|
#               |*D7**D8**D9*|
#               |************|

class Plantilla(ThreeDScene):

#Por añadir situación de bloques
    def __init__(self, algorithm, initial_state="UUUUUUUUURRRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB",
                 color_scheme = [WHITE, "#B90000", "#009B48", "#FFD500", "#FF5900", "#0045AD"], time_of_rotation = 2,
                 time_of_turns = 1, **kwargs):

        super().__init__(**kwargs)

        """PARTE 1: CREACIÓN DEL CUBO Y SU CONFIGURACIÓN ORIGINAL"""
        # Crear el cubo de Rubik, establecer patrón de colores y escalarlo
        # Patrón por defecto es correcto (se ha ajustado la cámara para ello)

        self.cube = RubiksCube(colors=color_scheme).scale(0.7)
        self.cube.set_state(initial_state)

        self.algorithm = algorithm
        self.time_of_rotation = time_of_rotation
        self.time_of_turns = time_of_turns



    def construct(self):

        """PARTE 2: COLOCACIÓN DEL CUBO EN PANTALLA"""

        # Configurar posiciones y cámara
        self.cube.move_to(ORIGIN)
        self.move_camera(phi=54.75*DEGREES, theta=(45+180)*DEGREES)
        self.renderer.camera.frame_center = self.cube.get_center()

        # Mostrar por pantalla con animación "FadeIn"
        self.play(FadeIn(self.cube))
        c = self.cube.cubies[0,0,0]

        self.wait(2)


        #Rotar alrededor de la diagonal
        self.play(Rotate(self.cube, angle=2*PI, axis=[-1,-1,-1], run_time=self.time_of_rotation, rate_func=linear))

        self.wait(1)

        """PARTE 3: EJECUTAR LOS MOVIMIENTOS QUE RESUELVEN EL CUBO"""

        list_of_moves = self.algorithm.split(" ")
        list_of_moves = [s for s in list_of_moves if s.strip()] #Deletes empty moves

        for m in list_of_moves:
            t = self.time_of_turns
            if m[-1] == '2':
                t = 1.75 * self.time_of_turns
            self.play(CubeMove(self.cube, m), run_time=t)

        #Terminar rotando sobre la diagonal
        self.play(Rotate(self.cube, angle=2 * PI, axis=[-1, -1, -1], run_time=self.time_of_rotation/2, rate_func=linear))

        self.wait(2)



        self.play(FadeOut(self.cube))


""" GENERACIÓN DEL VÍDEO: """

#CALLING EXAMPLE: 
# $ python manim_cube_visualizator.py 10 1.5 "low_quality"  "UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB" "WHITE,#B90000,#009B48,#FFD500,#FF5900,#0045AD" "R' U F2" 

config.quality = "low_quality"
#config.quality = "high_quality"

arg_tRotation = int(sys.argv[1])
arg_tMove = float(sys.argv[2])
arg_quality = sys.argv[3]
config.quality = arg_quality
arg_initial_state = sys.argv[4]
list_colours = sys.argv[5].split(",")
arg_moves = sys.argv[6]

sc = Plantilla(initial_state=arg_initial_state,
               algorithm=arg_moves, 
               color_scheme=list_colours,
            time_of_rotation=arg_tRotation, time_of_turns= arg_tMove)

sc.render(preview=True)

# [WHITE, "#B90000", "#009B48", "#FFD500", "#FF5900", "#0045AD"]
# [WHITE, RED, GREEN, YELLOW, ORANGE, DARK_BLUE]
# U R F D L B










"""
Se puede: generar un cubo solo del bandaged, el "cube shape".
Cada cara de una pieza son 4 puntos. 
Se puede intentar acceder a la piezas unidas, sacar sus puntos, eliminar los duplicados y generar paralelepípedo con los diferentes.

Lo complejo es averiguar los colores y hacer los movimientos.       
"""