package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.flag.FlagValue;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * The DimensionalRegion represents the only direct implementation of an Abstract region.
 * It is intended to be used to protect dimensions (vanilla and modded).
 */
public final class DimensionalRegion extends ProtectedRegion {

    public static final Codec<DimensionalRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            ResourceKey.codec(Registries.DIMENSION)
                                    .fieldOf("dim")
                                    .forGetter(ProtectedRegion::getDim),
                            UUIDUtil.STRING_CODEC.fieldOf("id")
                                    .forGetter(ProtectedRegion::getUuid),
                            UUIDUtil.STRING_CODEC.fieldOf("parentId")
                                    .forGetter(ProtectedRegion::getParentId),
                            Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("active")
                                    .forGetter(ProtectedRegion::isActive),
                            Codec.BOOL.fieldOf("muted")
                                    .forGetter(ProtectedRegion::isMuted),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC).fieldOf("groups")
                                    .forGetter(ProtectedRegion::getGroups),
                            Codec.list(UUIDUtil.STRING_CODEC).fieldOf("childrenIds")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenIds()))
                    )
                    .apply(instance, (dim, id, parentId, flags,
                                      isActive, isMuted, groups, childrenIds) ->
                            new DimensionalRegion(dim, id, parentId, new RegionFlags(flags), isActive, isMuted, groups, childrenIds)
                    )
    );

    public DimensionalRegion(ResourceKey<Level> levelRk, UUID id, UUID parentId) {
        super(levelRk.identifier().toString(), id, parentId, levelRk, RegionType.DIMENSION);
        this.dimension = levelRk;
    }

    private DimensionalRegion(ResourceKey<Level> levelRk, UUID id, UUID parentId, RegionFlags flags, boolean isActive,
                              boolean isMuted, Map<String, PlayerContainer> groups, List<UUID> childrenNames) {
        this(levelRk, id, parentId);
        this.dimension = levelRk;
        this.setFlags(flags);
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenIds(childrenNames);
    }

    @Override
    public Identifier getId() {
        return this.dimension.identifier();
    }

    @Override
    public String getName() {
        return this.dimension.identifier().toString();
    }
}
