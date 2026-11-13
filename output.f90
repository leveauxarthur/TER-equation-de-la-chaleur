module output
    use precision
    implicit none

contains

    subroutine sortie(iteration, T, coord_noeud, noeud_maille)
        integer, intent(in) :: iteration
        real(pr), intent(in) :: T(:)
        real(pr), intent(in) :: coord_noeud(:,:) ! Dimensions attendues : (nb_noeuds, 2)
        integer, intent(in) :: noeud_maille(:,:) ! Dimensions attendues : (nb_mailles, sommets_max)

        integer :: nb_mailles, nb_noeuds
        integer :: i, k, nb_sommets_reel, taille_liste
        character(len=50) :: filename

        ! Récupération automatique des dimensions via les tableaux
        nb_mailles = size(T)
        nb_noeuds = size(coord_noeud, 1)

        ! 1. Création dynamique du nom de fichier (ex: resultat_0001.vtk)
        write(filename, '("resultat_", I4.4, ".vtk")') iteration

        open(unit=20, file=filename, status='replace', action='write')

        ! ==========================================
        ! 2. EN-TÊTE VTK STANDARD
        ! ==========================================
        write(20, '(A)') "# vtk DataFile Version 3.0"
        write(20, '(A)') "Champ de Temperature - Maillage Voronoi"
        write(20, '(A)') "ASCII"
        write(20, '(A)') "DATASET UNSTRUCTURED_GRID"

        ! ==========================================
        ! 3. COORDONNÉES DES NŒUDS (POINTS)
        ! ==========================================
        ! VTK exige la 3D. On force donc la composante Z à 0.0
        write(20, '("POINTS ", I0, " float")') nb_noeuds
        do i = 1, nb_noeuds
            write(20, '(3(E15.6, 1X))') coord_noeud(i, 1), coord_noeud(i, 2), 0.0_pr
        end do

        ! ==========================================
        ! 4. CONNECTIVITÉ DES MAILLES (CELLS)
        ! ==========================================
        ! Pour VTK, la taille de la liste = (nb_mailles) + (somme totale des sommets de toutes les mailles)
        taille_liste = 0
        do i = 1, nb_mailles
            nb_sommets_reel = count(noeud_maille(i, :) /= 0)
            taille_liste = taille_liste + 1 + nb_sommets_reel
        end do

        write(20, '("CELLS ", I0, " ", I0)') nb_mailles, taille_liste
        
        do i = 1, nb_mailles
            nb_sommets_reel = count(noeud_maille(i, :) /= 0)
            ! ATTENTION CRITIQUE : VTK compte à partir de 0, Fortran à partir de 1 !
            ! Il faut donc soustraire 1 à tous les indices des nœuds.
            write(20, '(I0, 100(1X, I0))') nb_sommets_reel, &
                                          (noeud_maille(i, k) - 1, k = 1, nb_sommets_reel)
        end do

        ! ==========================================
        ! 5. TYPES DE CELLULES (CELL_TYPES)
        ! ==========================================
        ! Le code VTK "7" correspond à VTK_POLYGON (polygone 2D arbitraire)
        write(20, '("CELL_TYPES ", I0)') nb_mailles
        do i = 1, nb_mailles
            write(20, '(I0)') 7
        end do

        ! ==========================================
        ! 6. DONNÉES PHYSIQUES (CELL_DATA)
        ! ==========================================
        write(20, '("CELL_DATA ", I0)') nb_mailles
        write(20, '(A)') "SCALARS Temperature float 1"
        write(20, '(A)') "LOOKUP_TABLE default"
        do i = 1, nb_mailles
            write(20, '(E15.6)') T(i)
        end do

        close(20)
        
        print *, "Fichier VTK exporte : ", trim(filename)

    end subroutine sortie

end module output