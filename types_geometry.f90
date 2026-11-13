module types_geometry
    use precision
    implicit none 



    type :: Point
        real(pr) :: x
        real(pr) :: y
    end type Point


    type :: Triangle
        integer :: vertex(3) ! utilisation d'identifiants pour definir les sommets des triangles (sommet n°i de coordonées (x,y))
        integer :: neighbor(3) ! convention : l'arete numero i est celle qui est opposée au sommet i (0 si on est au bor et que le triangle ne partage pas d'arete avec un autre triangle)
        logical :: active = .true. ! regarde si le triangle existe toujours ou a été supprimé
    end type Triangle

    type :: Edge
        integer :: vertex(2) ! utilisation d'identifiants pour definir les 2 points qui constituent l'arete
        integer :: outside_triangle ! contient le triangle qui est a la frontiere de la cavité, utile dans l'algo plus tard 
    end type Edge

    type :: Mesh
    type(Point), allocatable :: points(:)
    type(Triangle), allocatable :: triangles(:)

    integer :: nb_points = 0
    integer :: nb_triangles = 0
    end type Mesh




end module types_geometry