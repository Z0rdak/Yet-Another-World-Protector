package de.z0rdak.yawp.platform.services.config;

import de.z0rdak.yawp.commands.CommandSourceType;
import net.minecraft.commands.CommandSourceStack;

import java.util.Set;

public interface IFlagConfigHelper {

    Set<String> getCoveredBlockEntities();
    Set<String> getCoveredBlockEntityTags();
    boolean removeEntitiesEnabled();
}
