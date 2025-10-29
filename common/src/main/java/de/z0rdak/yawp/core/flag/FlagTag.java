package de.z0rdak.yawp.core.flag;

import net.minecraft.resources.ResourceLocation;
import org.jetbrains.annotations.Nullable;

public record FlagTag(ResourceLocation tagRl, @Nullable String description) { }
