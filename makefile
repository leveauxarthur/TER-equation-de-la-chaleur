FC = gfortran
FFLAGS = -Wall -Wextra -Wpedantic -fcheck=all -g -O0

BUILD = build
TARGET = programme

.PHONY: all run clean

all: $(TARGET)

$(BUILD):
	mkdir -p $(BUILD)

$(BUILD)/precision.o: precision.f90 | $(BUILD)
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/types_geometry.o: types_geometry.f90 $(BUILD)/precision.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/predicates.o: predicates.f90 $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/sorting.o: sorting.f90 $(BUILD)/precision.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/walk.o: walk.f90 $(BUILD)/predicates.o $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/cavity.o: cavity.f90 $(BUILD)/predicates.o $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/delaunay_ball.o: delaunay_ball.f90 $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/delaunay.o: delaunay.f90 $(BUILD)/walk.o $(BUILD)/cavity.o $(BUILD)/delaunay_ball.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/mesh_tools.o: mesh_tools.f90 $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/boundary_conditions.o: boundary_conditions.f90 $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/thermal_solver.o: thermal_solver.f90 $(BUILD)/types_geometry.o $(BUILD)/boundary_conditions.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

$(BUILD)/output.o: output.f90 $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

# Règle pour le nouveau module de post-traitement
$(BUILD)/post_traitement.o: post_traitement.f90 $(BUILD)/precision.o $(BUILD)/types_geometry.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

# Le main dépend de post_traitement (et des autres modules)
$(BUILD)/main.o: main.f90 $(BUILD)/precision.o $(BUILD)/types_geometry.o $(BUILD)/delaunay.o $(BUILD)/thermal_solver.o $(BUILD)/output.o $(BUILD)/post_traitement.o
	$(FC) $(FFLAGS) -J$(BUILD) -I$(BUILD) -c $< -o $@

# Édition des liens avec tous les objets générés
$(TARGET): $(BUILD)/precision.o $(BUILD)/types_geometry.o $(BUILD)/predicates.o $(BUILD)/sorting.o $(BUILD)/walk.o $(BUILD)/cavity.o $(BUILD)/delaunay_ball.o $(BUILD)/delaunay.o $(BUILD)/mesh_tools.o $(BUILD)/boundary_conditions.o $(BUILD)/thermal_solver.o $(BUILD)/output.o $(BUILD)/post_traitement.o $(BUILD)/main.o
	$(FC) $(FFLAGS) -o $@ $^

run: $(TARGET)
	./$(TARGET)

clean:
	rm -rf $(BUILD) $(TARGET) *.mod *.o