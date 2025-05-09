package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.constants.serialization.RegionNbtKeys;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.List;
import java.util.Map;

public final class SphereRegion extends MarkedRegion {

    public SphereRegion(String name, SphereArea area, ResourceKey<Level> dimension) {
        this(name, area, area.getCenterPos(), null, dimension);
    }

    public SphereRegion(String name, SphereArea area, Player owner, ResourceKey<Level> dimension) {
        super(name, area, area.getCenterPos(), owner, dimension);
    }

    public SphereRegion(String name, SphereArea area, BlockPos tpPos, Player player, ResourceKey<Level> dimension) {
        super(name, area, tpPos, player, dimension);
    }

    public SphereRegion(String name, ResourceKey<Level> dim, String parentName, Map<String, IFlag> flags,
                        boolean isActive, boolean isMuted, int priority, IMarkableArea area, BlockPos blockPos,
                        Map<String, PlayerContainer> groups, List<String> childrenNames){
        super(name, dim, parentName, flags, isActive, isMuted, priority, AreaType.SPHERE.areaType, area, blockPos, groups, childrenNames);
    }
}
