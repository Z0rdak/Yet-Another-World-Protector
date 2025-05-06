package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.DefaultedRegistry;
import net.minecraft.core.MappedRegistry;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public record MarkedAreaType<T extends IMarkableArea>(MapCodec<T> codec) {

    public static final Registry<MarkedAreaType<?>> REGISTRY = new MappedRegistry<>(
            ResourceKey.createRegistryKey(ResourceLocation.parse("yawp:marked_area_types")), Lifecycle.stable());

    public static ResourceLocation areaIdentifier(AreaType areaType) {
        return ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, areaType.toString().toLowerCase(Locale.ROOT));
    }

    public static Codec<IMarkableArea> MARKED_AREA_CODEC = MarkedAreaType.REGISTRY.byNameCodec()
            .dispatch("areaType", IMarkableArea::getType, MarkedAreaType::codec);
}
