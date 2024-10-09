package de.z0rdak.yawp.platform.services.config;

import de.z0rdak.yawp.commands.CommandSourceType;
import net.minecraft.commands.CommandSourceStack;

import java.util.Set;

public interface IRegionConfigHelper {

    boolean shouldActivateNewDimRegion();
    int getPaginationSize();
    int getDefaultPriority();
    int getDefaultPriorityInc();
    Set<String> getDefaultFlags();
    Set<String> getDefaultDimFlags();
}
