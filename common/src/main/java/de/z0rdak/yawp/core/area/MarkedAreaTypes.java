package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import de.z0rdak.yawp.YAWPCommon;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.MappedRegistry;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;

import java.util.Locale;

public class MarkedAreaTypes {

    public static Identifier areaIdentifier(AreaType areaType) {
        return Identifier.fromNamespaceAndPath(Constants.MOD_ID, areaType.toString().toLowerCase(Locale.ROOT));
    }

    public static Registry<MarkedAreaType<?>> REGISTRY = new MappedRegistry<>(
            ResourceKey.createRegistryKey(Identifier.fromNamespaceAndPath(Constants.MOD_ID, "marked_area_types")), Lifecycle.stable());

    public static final MarkedAreaType<CuboidArea> CUBOID_AREA = register("cuboid", new MarkedAreaType<>(CuboidArea.CODEC));
    public static final MarkedAreaType<SphereArea> SPHERE_AREA = register("sphere", new MarkedAreaType<>(SphereArea.CODEC));

    public static Codec<IMarkableArea> MARKED_AREA_CODEC = REGISTRY.byNameCodec()
            .dispatch("areaType", IMarkableArea::getType, MarkedAreaType::codec);

    public static <T extends MarkedArea> MarkedAreaType<T> register(String id, MarkedAreaType<T> areaType) {
        return Registry.register(REGISTRY, Identifier.fromNamespaceAndPath(Constants.MOD_ID, id), areaType);
    }

}
