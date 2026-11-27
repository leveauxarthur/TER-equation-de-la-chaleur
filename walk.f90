module walk
    use precision
    use geometry_types
    use predicates
    implicit none

    private
    public :: localiser_triangle

contains

    !> Localise le triangle actif contenant le point p_cible à partir de tri_depart.
    !> Convention : l'arête locale i (1..3) est opposée au sommet i.
    function localiser_triangle(maillage, p_cible, tri_depart) result(tri_courant)
        type(Mesh), intent(in)  :: maillage
        type(Point), intent(in) :: p_cible
        integer, intent(in)     :: tri_depart
        integer                 :: tri_courant

        integer :: i, s1, s2, tri_suivant, pas, pas_max
        real(pr) :: cote
        type(Point) :: a, b
        type(Triangle) :: tri

        tri_courant = tri_depart
        if (tri_courant < 1 .or. tri_courant > maillage%nb_triangles) then
            tri_courant = trouver_premier_triangle_actif(maillage)
        end if

        pas_max = max(100, 2 * maillage%nb_triangles)

        do pas = 1, pas_max
            if (.not. maillage%triangles(tri_courant)%active) then
                tri_courant = trouver_premier_triangle_actif(maillage)
                if (tri_courant == 0) return
            end if

            tri = maillage%triangles(tri_courant)
            tri_suivant = 0

            ! Teste les 3 arêtes du triangle courant
            do i = 1, 3
                call sommets_arete_ccw(tri, i, s1, s2)
                a = maillage%points(s1)
                b = maillage%points(s2)

                ! Si p_cible est à droite de l'arête (a -> b), il est hors du triangle de ce côté
                cote = orient2d(a, b, p_cible)
                if (cote < 0.0_pr) then
                    tri_suivant = tri%neighbor(i)
                    exit
                end if
            end do

            ! Aucune arête ne rejette p_cible (ou voisin inexistant / bord)
            if (tri_suivant > 0) then
                if (maillage%triangles(tri_suivant)%active) then
                    tri_courant = tri_suivant
                else
                    return
                end if
            else
                return
            end if
        end do

    end function localiser_triangle


    !> Renvoie les indices des deux sommets (s1 -> s2) de l'arête locale num_arete
    !> en respectant l'ordre cyclique (1, 2, 3) du triangle.
    subroutine sommets_arete_ccw(tri, num_arete, s1, s2)
        type(Triangle), intent(in) :: tri
        integer, intent(in)        :: num_arete
        integer, intent(out)       :: s1, s2

        ! Arête 1 opposée au sommet 1 -> sommets 2 et 3
        ! Arête 2 opposée au sommet 2 -> sommets 3 et 1
        ! Arête 3 opposée au sommet 3 -> sommets 1 et 2
        select case (num_arete)
        case (1)
            s1 = tri%vertex(2)
            s2 = tri%vertex(3)
        case (2)
            s1 = tri%vertex(3)
            s2 = tri%vertex(1)
        case (3)
            s1 = tri%vertex(1)
            s2 = tri%vertex(2)
        case default
            s1 = tri%vertex(1)
            s2 = tri%vertex(2)
        end select
    end subroutine sommets_arete_ccw


    !> Secours : retourne l'indice du premier triangle actif, ou 0 s'il n'y en a aucun.
    function trouver_premier_triangle_actif(maillage) result(indice)
        type(Mesh), intent(in) :: maillage
        integer :: indice, k

        indice = 0
        do k = 1, maillage%nb_triangles
            if (maillage%triangles(k)%active) then
                indice = k
                return
            end if
        end do
    end function trouver_premier_triangle_actif

end module walk
