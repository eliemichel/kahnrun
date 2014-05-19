
import bpy

f = open("/tmp/export.pov", "w")

for obj in bpy.context.selected_objects:
    for p in obj.data.polygons:
        f.write("smooth_triangle {\n")
        for i in range(3):
            v = obj.data.loops[p.loop_start + i].vertex_index
            x, y, z = obj.data.vertices[v].co
            nx, ny, nz = obj.data.vertices[v].normal
            f.write("  <%f, %f, %f>, <%f, %f, %f>" % (x, y, z, nx, ny, nz))
            f.write(",\n" if i < 2 else "\n")
        f.write("}\n\n")

f.close()



