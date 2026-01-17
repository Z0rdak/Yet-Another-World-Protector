package de.z0rdak.yawp.core.flag;

import net.minecraft.resources.Identifier;
import org.jetbrains.annotations.Nullable;

public record FlagTag(Identifier tagRl, @Nullable String description) { }
