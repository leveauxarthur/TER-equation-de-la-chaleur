program main

    use precision
    use geometry_types
    ! use delaunay           
    ! use boundary_conditions
    ! use thermal_solver
    ! use output
    use post_traitement

    implicit none

    ! ==========================================
    ! 1. DECLARATIONS
    ! ==========================================
    real(pr) :: T_init, T_g, T_d, phi_b, cfl, t_max, D, dt
    integer  :: N, j
    character(len=30) :: fichier_maillage
    real(pr) :: temps_debut, temps_fin

    integer :: nb_mailles, nb_aretes
    real(pr), allocatable :: l_arete(:), d_arete(:), aire_maille(:)
    integer, allocatable :: maille_arete(:,:), cl_arete(:)
    real(pr), allocatable :: coord_noeud(:,:)
    integer, allocatable :: noeud_maille(:,:)

    integer :: nnz, i, k, m1, m2
    real(pr) :: coef
    ! b contient les CL stationnaires, b_imp est le RHS réactualisé à chaque pas de temps
    real(pr), allocatable :: val(:), b(:), b_imp(:), T(:), Tnp1(:)
    integer, allocatable :: col_ind(:), row_ptr(:), ptr_temp(:)


    ! ==========================================
    ! 2. LECTURE DES PARAMETRES
    ! ==========================================
    open(10, file='parametres.dat', status='old')
    read(10,*) T_init, T_g, T_d, phi_b, cfl, t_max, fichier_maillage, D
    close(10)
    
    ! Paramètres temporels forcés pour le test
    dt = 0.1_pr
    N = int(t_max / dt)


    ! ==========================================
    ! 3. MAILLAGE (BOUCHONNAGE)
    ! ==========================================
    nb_mailles = 2
    nb_aretes = 7
    allocate(l_arete(nb_aretes), d_arete(nb_aretes), aire_maille(nb_mailles))
    allocate(maille_arete(nb_aretes, 2), cl_arete(nb_aretes))

    aire_maille = 0.5_pr ! Aires fictives
    maille_arete(1,:) = (/ 1, 2 /) ; l_arete(1) = 1.0_pr ; d_arete(1) = 0.5_pr ; cl_arete(1) = 0
    maille_arete(2,:) = (/ 1, 0 /) ; l_arete(2) = 1.0_pr ; d_arete(2) = 0.25_pr ; cl_arete(2) = 10
    maille_arete(3,:) = (/ 2, 0 /) ; l_arete(3) = 1.0_pr ; d_arete(3) = 0.25_pr ; cl_arete(3) = 11
    maille_arete(4,:) = (/ 1, 0 /) ; l_arete(4) = 0.5_pr ; d_arete(4) = 0.25_pr ; cl_arete(4) = 20
    maille_arete(5,:) = (/ 1, 0 /) ; l_arete(5) = 0.5_pr ; d_arete(5) = 0.25_pr ; cl_arete(5) = 20
    maille_arete(6,:) = (/ 2, 0 /) ; l_arete(6) = 0.5_pr ; d_arete(6) = 0.25_pr ; cl_arete(6) = 20
    maille_arete(7,:) = (/ 2, 0 /) ; l_arete(7) = 0.5_pr ; d_arete(7) = 0.25_pr ; cl_arete(7) = 20


    ! ==========================================
    ! 4. CREATION DU SQUELETTE CSR
    ! ==========================================
    allocate(ptr_temp(nb_mailles))
    ptr_temp = 1 
    do k = 1, nb_aretes
        m1 = maille_arete(k, 1)
        m2 = maille_arete(k, 2)
        if (m2 /= 0) then
            ptr_temp(m1) = ptr_temp(m1) + 1
            ptr_temp(m2) = ptr_temp(m2) + 1
        end if
    end do

    allocate(row_ptr(nb_mailles + 1))
    row_ptr(1) = 1
    do i = 1, nb_mailles
        row_ptr(i+1) = row_ptr(i) + ptr_temp(i)
    end do
    nnz = row_ptr(nb_mailles + 1) - 1

    allocate(val(nnz), col_ind(nnz), b(nb_mailles), b_imp(nb_mailles))
    allocate(T(nb_mailles), Tnp1(nb_mailles))
    
    ptr_temp = row_ptr(1:nb_mailles)
    
    ! Placement garanti des diagonales en premier élément de chaque ligne
    do i = 1, nb_mailles
        col_ind(ptr_temp(i)) = i
        ptr_temp(i) = ptr_temp(i) + 1
    end do

    do k = 1, nb_aretes
        m1 = maille_arete(k, 1)
        m2 = maille_arete(k, 2)
        if (m2 /= 0) then
            col_ind(ptr_temp(m1)) = m2 ; ptr_temp(m1) = ptr_temp(m1) + 1
            col_ind(ptr_temp(m2)) = m1 ; ptr_temp(m2) = ptr_temp(m2) + 1
        end if
    end do


    ! ==========================================
    ! 5. ASSEMBLAGE STATIONNAIRE (HORS BOUCLE)
    ! ==========================================
    val = 0.0_pr
    b   = 0.0_pr
    T   = T_init

    do k = 1, nb_aretes
        m1 = maille_arete(k, 1)
        m2 = maille_arete(k, 2)
        coef = D * (l_arete(k) / d_arete(k))

        if (m2 /= 0) then
            call add_to_csr(m1, m1, coef)   
            call add_to_csr(m2, m2, coef)   
            call add_to_csr(m1, m2, -coef)  
            call add_to_csr(m2, m1, -coef)  
        else
            if (cl_arete(k) == 10) then        
                call add_to_csr(m1, m1, coef)
                b(m1) = b(m1) + coef * T_g
            else if (cl_arete(k) == 11) then   
                call add_to_csr(m1, m1, coef)
                b(m1) = b(m1) + coef * T_d
            end if
        end if
    end do

    ! INERTIE IMPLICITE : Ajout de (Aire / dt) sur la diagonale de la matrice A
    ! Grâce à l'étape 4, la diagonale de la ligne 'i' est toujours à l'indice row_ptr(i)
    do i = 1, nb_mailles
        val(row_ptr(i)) = val(row_ptr(i)) + (aire_maille(i) / dt)
    end do


    ! ==========================================
    ! 6. RESOLUTION DYNAMIQUE (BOUCLE EN TEMPS)
    ! ==========================================
    call cpu_time(temps_debut)

    do j = 1, N
        ! Actualisation du second membre avec l'inertie de l'instant précédent
        do i = 1, nb_mailles
            b_imp(i) = b(i) + (aire_maille(i) / dt) * T(i)
        end do

        ! --- APPEL DU SOLVEUR ---
        ! call solveur_gradient_conjugue(nnz, nb_mailles, val, col_ind, row_ptr, b_imp, Tnp1)
        
        ! Bouchon du solveur : convergence arbitraire pour tester la boucle
        Tnp1 = b_imp / val(row_ptr(1:nb_mailles)) 

        ! Mise à jour du champ de température
        T = Tnp1

        ! Post-traitement et extraction ParaView périodique
        if (mod(j, max(1, N/10)) == 0) then
            ! call calculer_erreur(T, aire_maille, coord_noeud, noeud_maille, nb_mailles)
            ! call sortie(j, T, coord_noeud, noeud_maille)
        end if
    end do

    call cpu_time(temps_fin)
    print *, "Temps de resolution implicite : ", temps_fin - temps_debut, " sec"
    print *, "Temperature finale T          : ", T

    deallocate(val, col_ind, row_ptr, b, b_imp, T, Tnp1)
    deallocate(l_arete, d_arete, maille_arete, cl_arete, ptr_temp, aire_maille)

contains

    subroutine add_to_csr(row, col, v)
        integer, intent(in) :: row, col
        real(pr), intent(in) :: v
        integer :: idx
        do idx = row_ptr(row), row_ptr(row+1) - 1
            if (col_ind(idx) == col) then
                val(idx) = val(idx) + v
                return
            end if
        end do
    end subroutine add_to_csr

end program main
