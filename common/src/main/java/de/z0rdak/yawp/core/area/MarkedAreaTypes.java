package de.z0rdak.yawp.core.area;

import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;

public class MarkedAreaTypes {

    public static final MarkedAreaType<CuboidArea> CUBOID_AREA = register("cuboid", new MarkedAreaType<>(CuboidArea.CODEC));
    public static final MarkedAreaType<SphereArea> SPHERE_AREA = register("sphere", new MarkedAreaType<>(SphereArea.CODEC));

    public static <T extends MarkedArea> MarkedAreaType<T> register(String id, MarkedAreaType<T> areaType) {
        return Registry.register(MarkedAreaType.REGISTRY, ResourceLocation.fromNamespaceAndPath("yawp", id), areaType);
    }

}
