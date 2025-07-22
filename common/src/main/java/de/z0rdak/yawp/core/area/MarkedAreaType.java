package de.z0rdak.yawp.core.area;

import com.mojang.serialization.Codec;
import com.mojang.serialization.MapCodec;

public record MarkedAreaType<T extends IMarkableArea>(Codec<T> codec) {
}
