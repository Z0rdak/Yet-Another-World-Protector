package de.z0rdak.yawp.api;

import de.z0rdak.yawp.core.flag.FlagMetaInfo;
import net.minecraft.resources.ResourceLocation;

public record Flag(ResourceLocation id, FlagMetaInfo flagInfo) {

    public String name() {
        return id.toString();
    }
}
