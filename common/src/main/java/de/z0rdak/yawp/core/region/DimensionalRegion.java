package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.flag.Flag;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends ProtectedRegion {

    public static final Codec<DimensionalRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            Codec.STRING.fieldOf("name")
                                    .forGetter(ProtectedRegion::getName),
                            ResourceKey.codec(Registries.DIMENSION)
                                    .fieldOf("dimension")
                                    .forGetter(ProtectedRegion::getDim),
                            Codec.STRING.fieldOf("parentName")
                                    .forGetter(ProtectedRegion::getParentName),
                            Codec.STRING.fieldOf("type")
                                    .forGetter(r -> r.getRegionType().type),
                            Codec.unboundedMap(Codec.STRING, Flag.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("isActive")
                                    .forGetter(ProtectedRegion::isActive),
                            Codec.BOOL.fieldOf("isMuted")
                                    .forGetter(ProtectedRegion::isMuted),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC).fieldOf("groups")
                                    .forGetter(ProtectedRegion::getGroups),
                            Codec.list(Codec.STRING).fieldOf("childrenNames")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenNames()))
                    )
                    .apply(instance, (name, dim, parentName, regionType,
                                      flags, isActive, isMuted, groups, childrenNames) ->
                            new DimensionalRegion(dim, new RegionFlags(flags), isActive, isMuted, groups, childrenNames)
                    )
    );

    public DimensionalRegion(ResourceKey<Level> dimensionKey, IProtectedRegion parent) {
        super(dimensionKey.location().toString(), dimensionKey, RegionType.DIMENSION);
        this.dimension = dimensionKey;
        if (!(parent instanceof GlobalRegion)) {
            throw new IllegalArgumentException("Illegal parent region for dimensional region");
        }
        this.setParent(parent);
    }

    private DimensionalRegion(ResourceKey<Level> dim, RegionFlags flags, boolean isActive, boolean isMuted, Map<String, PlayerContainer> groups, List<String> childrenNames) {
        super(dim.location().toString(), dim, RegionType.DIMENSION);
        this.dimension = dim;
        var globalRegion = RegionDataManager.getGlobalRegion();
        this.setParent(globalRegion);
        this.setFlags(flags);
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenNames(childrenNames);
    }

    @Override
    protected boolean setParent(IProtectedRegion parent) {
        if (parent.getRegionType() == RegionType.GLOBAL) {
            return super.setParent(parent);
        }
        return false;
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child.getRegionType() == RegionType.LOCAL && child.getParent() == null) {
            String parentName = child.getParentName();
            if (parentName != null && !parentName.equals(this.getName())) {
                super.addChild(child);
                ((ProtectedRegion) child).parentName = parentName;
                return true;
            }
            return super.addChild(child);
        }
        if (child.getRegionType() == RegionType.LOCAL && child.getParent().getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        if (child.getRegionType() == RegionType.LOCAL && !child.getParent().hasChild(child)) {
            return super.addChild(child);
        }
        return false;
    }
    @Override
    public String getName() {
        return this.dimension.location().toString();
    }
}
