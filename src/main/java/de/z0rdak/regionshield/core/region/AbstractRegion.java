package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.affiliation.PlayerContainer;
import de.z0rdak.regionshield.core.flag.FlagContainer;
import de.z0rdak.regionshield.core.flag.IFlag;
import de.z0rdak.regionshield.core.flag.RegionFlag;
import de.z0rdak.regionshield.util.PlayerUtils;
import de.z0rdak.regionshield.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;

import java.util.*;
import java.util.regex.Pattern;

/**
 * A abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions).
 */
public abstract class AbstractRegion implements IProtectedRegion {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z0-9\\-+]+$");
    private String name;
    private FlagContainer flags;
    private PlayerContainer owners;
    private PlayerContainer members;
    private boolean isActive;

    // TODO: (command) define region with members/owners
    protected AbstractRegion(String name) {
        this.name = name;
        this.flags = new FlagContainer();
        this.members = new PlayerContainer();
        this.owners = new PlayerContainer();
        this.isActive = true;
    }

    /**
     * Minimal constructor to create a abstract region by supplying a name and an owner.
     * @param name name of the region
     * @param owner region owner
     */
    protected AbstractRegion(String name, PlayerEntity owner) {
        this(name);
        this.owners.players.put(owner.getUUID(), owner.getScoreboardName());
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public boolean addFlag(IFlag flag){
        return this.flags.put(flag.getFlagName(), flag) == null;
    }

    public boolean containsFlag(IFlag flag) {
        return this.flags.containsKey(flag.getFlagName());
}

    @Override
    public boolean removeFlag(IFlag flag) {
        return this.flags.remove(flag.getFlagName()) != null;
    }

    public boolean containsFlag(RegionFlag flag) {
        return this.flags.containsKey(flag.flagname);
    }

    @Override
    public Collection<IFlag> getFlags() {
        return Collections.unmodifiableList(new ArrayList<>(this.flags.values()));
    }

    @Override
    public boolean isActive() {
        return this.isActive;
    }

    @Override
    public void setIsActive(boolean isActive) {
        this.isActive = isActive;
    }

    @Override
    public boolean addMember(PlayerEntity player) {
        return false;
    }

    @Override
    public boolean addOwner(PlayerEntity player) {
        return false;
    }

    // TODO: rethink return values - are they needed? they complicate things even more
    @Override
    public boolean removeMember(UUID uuid) {
        this.members.players.remove(uuid);
        return true;
    }

    @Override
    public boolean removeOwner(UUID uuid) {
        return false;
    }

    @Override
    public PlayerContainer getMembers() {
        return this.members;
    }

    @Override
    public PlayerContainer getOwners() {
        return owners;
    }

    /**
     * Checks if the player is defined in the regions player list OR whether the player is an operator.
     * Usually this check is needed when an event occurs and it needs to be checked whether
     * the player has a specific permission to perform an action in the region.
     *
     * @param player to be checked
     * @return true if player is in region list or is an operator, false otherwise
     */
    // TODO: check
    @Override
    public boolean permits(PlayerEntity player) {
        if (PlayerUtils.hasPermission(player) || this.owners.containsPlayer(player.getUUID()) || this.owners.containsPlayer(player.getUUID())) {
            return true;
        }
        return members.containsPlayer(player.getUUID());
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(RegionNBT.NAME, this.name);
        nbt.putBoolean(RegionNBT.ACTIVE, this.isActive);
        nbt.put(RegionNBT.FLAGS, this.flags.serializeNBT());
        nbt.put(RegionNBT.OWNERS, this.owners.serializeNBT());
        nbt.put(RegionNBT.MEMBERS, this.members.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.name = nbt.getString(RegionNBT.NAME);
        this.isActive = nbt.getBoolean(RegionNBT.ACTIVE);
        this.flags = new FlagContainer(nbt.getCompound(RegionNBT.FLAGS));
        this.owners = new PlayerContainer(nbt.getCompound(RegionNBT.OWNERS));
        this.members = new PlayerContainer(nbt.getCompound(RegionNBT.MEMBERS));
    }
}
