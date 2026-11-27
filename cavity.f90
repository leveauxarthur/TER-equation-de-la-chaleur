module cavity

    implicit none

    type :: BoundaryEdge
        integer :: tri_outside  ! indice du triangle voisin qui survit (hors cavité)
        integer :: local_edge   ! indice local de l'arête partagée (1, 2 ou 3)
    end type BoundaryEdge

contains

    subroutine build_cavity_2d(p_new, tri_start, n_tris, tri_neighbors, &
                               cavity_tris, n_cavity, boundary_edges, n_boundary)
        !=======================================================================
        ! p_new          : Coordonnées 2D (x, y) du point à insérer
        ! tri_start      : Indice du triangle contenant p_new (issu de WalK)
        ! n_tris         : Nombre total de triangles actuels
        ! tri_neighbors  : Tableau (3, n_tris) des 3 voisins de chaque triangle
        ! cavity_tris    : (Sortie) Indices des triangles à détruire
        ! boundary_edges : (Sortie) Arêtes de la frontière de la cavité
        ! n_cavity       : (Sortie) nombre de triangles dans la cavité
        ! n_boundary     : (Sortie) nombre de faces frontières
        !=======================================================================
        
        real, dimension(2), intent(in)              :: p_new
        integer, intent(in)           :: tri_start, n_tris
        integer, dimension(3, *), intent(in)           :: tri_neighbors
        
        integer, dimension(:), allocatable, intent(out) :: cavity_tris
        integer, intent(out)          :: n_cavity
        type(BoundaryEdge), dimension(:), allocatable, intent(out) :: boundary_edges
        integer, intent(out)          :: n_boundary

        ! Variables locales pour le parcours en largeur (BFS)
        integer, dimension(:), allocatable :: queue
        integer, dimension(:), allocatable :: state ! 0 = non testé, 1 = dans cavité, -1 = hors cavité
        integer :: head, tail
        integer :: current_tri, neighbor_tri
        integer :: i

        allocate(queue(n_tris))
        allocate(state(n_tris))
        allocate(cavity_tris(n_tris))
        allocate(boundary_edges(n_tris * 3))

        state = 0
        n_cavity = 0
        n_boundary = 0
        head = 1
        tail = 1

        ! 1. Amorçage de la file avec le triangle trouvé par WalK
        queue(tail) = tri_start
        state(tri_start) = 1
        tail = tail + 1

        ! 2. Parcours en largeur (BFS)
        do while (head < tail)
            current_tri = queue(head)
            head = head + 1
            
            n_cavity = n_cavity + 1
            cavity_tris(n_cavity) = current_tri

            ! On inspecte les 3 arêtes du triangle
            do i = 1, 3
                neighbor_tri = tri_neighbors(i, current_tri)

                ! Avec le point à l'infini p_inf, neighbor_tri n'est jamais 0
                if (state(neighbor_tri) == 0) then
                    
                    ! /!\ PRÉDICAT 2D : Cercle circonscrit /!\
                    if (inCircle(neighbor_tri, p_new)) then
                        state(neighbor_tri) = 1
                        queue(tail) = neighbor_tri
                        tail = tail + 1
                    else
                        state(neighbor_tri) = -1
                    end if
                end if

                ! Si le voisin est HORS de la cavité, l'arête commune est une frontière du trou
                if (state(neighbor_tri) == -1) then
                    n_boundary = n_boundary + 1
                    boundary_edges(n_boundary)%tri_outside = neighbor_tri
                    boundary_edges(n_boundary)%local_edge = find_local_edge(neighbor_tri, current_tri, tri_neighbors)
                end if
            end do
        end do

        deallocate(queue)
        deallocate(state)

    end subroutine build_cavity_2d

    ! Fonctions auxiliaires

    logical function inCircle(tri_index, p_new)
        integer, intent(in) :: tri_index
        real, dimension(2), intent(in)    :: p_new
        ! Calcul du déterminant 3x3 pour le cercle circonscrit
        ! Remarque : Si tri_index est un triangle fantôme (relié à p_inf),
        ! inCircle renvoie .true. si p_new est du côté extérieur de l'arête réelle.
        inCircle = .true. ! (À remplacer par ton prédicat géométrique 2D)
    end function inCircle

    integer function find_local_edge(tri_A, tri_B, tri_neighbors)
        integer, intent(in) :: tri_A, tri_B
        integer, dimension(3, *), intent(in) :: tri_neighbors
        integer :: i
        find_local_edge = 0
        do i = 1, 3
            if (tri_neighbors(i, tri_A) == tri_B) then
                find_local_edge = i
                exit
            end if
        end do
    end function find_local_edge

end module cavity
