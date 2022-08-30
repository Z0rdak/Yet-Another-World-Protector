package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.ConditionFlag;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;

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
    protected AbstractRegion(String name, Player owner) {
        this(name);
        this.owners.addPlayer(owner);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void addFlag(IFlag flag){
        this.flags.put(flag.getFlagName(), flag);
    }

    @Override
    public void addFlag(String flag){
        this.flags.put(flag, new ConditionFlag(flag, false));
    }

    public boolean containsFlag(IFlag flag) {
        return this.flags.containsKey(flag.getFlagName());
}

    @Override
    public void removeFlag(String flag) {
        this.flags.remove(flag);
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
    public void addMember(Player player) {
        this.members.addPlayer(player);
    }

    @Override
    public void addMember(Team team) {
        this.members.addTeam(team);
    }


    @Override
    public void addOwner(Player player) {
        this.owners.addPlayer(player);
    }

    @Override
    public void addOwner(Team team) {
        this.owners.addTeam(team);
    }

    @Override
    public void removeMember(Player player) {
        this.members.removePlayer(player);

    }

    @Override
    public void removeMember(Team team) {
        this.members.removeTeam(team.getName());

    }

    @Override
    public void removeOwner(Player player) {
        this.owners.removePlayer(player);
    }

    @Override
    public void removeOwner(Team team) {
        this.owners.removeTeam(team);

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
    @Override
    public boolean permits(Player player) {
        boolean isOwner = this.owners.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.owners.containsTeam(player.getTeam().getName()));
        boolean isMember = this.members.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.members.containsTeam(player.getTeam().getName()));
        return isOwner || isMember;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(RegionNBT.NAME, this.name);
        nbt.putBoolean(RegionNBT.ACTIVE, this.isActive);
        nbt.put(RegionNBT.FLAGS, this.flags.serializeNBT());
        nbt.put(RegionNBT.OWNERS, this.owners.serializeNBT());
        nbt.put(RegionNBT.MEMBERS, this.members.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString(RegionNBT.NAME);
        this.isActive = nbt.getBoolean(RegionNBT.ACTIVE);
        this.flags = new FlagContainer(nbt.getCompound(RegionNBT.FLAGS));
        this.owners = new PlayerContainer(nbt.getCompound(RegionNBT.OWNERS));
        this.members = new PlayerContainer(nbt.getCompound(RegionNBT.MEMBERS));
    }
}
